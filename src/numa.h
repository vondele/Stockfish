/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2024 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef NUMA_H_INCLUDED
#define NUMA_H_INCLUDED

#include <cstdlib>
#include <cstdint>
#include <vector>
#include <set>
#include <map>
#include <thread>
#include <string>
#include <memory>
#include <atomic>
#include <utility>

#include "misc.h"

namespace Stockfish {

// Designed as immutable, because there is no good reason to alter an already existing config
// in a way that doesn't require recreating it completely.
class NumaConfig {
public:
  using CpuIndex = size_t;
  using NumaIndex = size_t;

  NumaConfig() {
    const CpuIndex numCpus = CpuIndex{std::max(1, std::thread::hardware_concurrency())};
    add_cpu_range_to_node(NumaIndex{0}, CpuIndex{0}, numCpus-1);
  }

  static NumaConfig empty() {
    return NumaConfig(EmptyNodeTag{});
  }

  static NumaConfig from_system() {
    // TODO: winapi/lscpu
  }

  // ':'-separated numa nodes
  // ','-separated cpu indices
  // supports "first-last" range syntax for cpu indices, 
  static NumaConfig from_string(const std::string& s) {
    NumaConfig cfg = empty();

    NumaIndex n = 0;
    for (auto&& nodeStr : split(s, ":")) {
      for (const std::string& cpuStr : split(nodeStr, ",")) {
        if (cpuStr.empty())
          continue

        auto parts = split(cpuStr, "-");
        if (parts.size() == 1) {
          const CpuIndex c = CpuIndex{std::stoll(parts[0])};
          if (!cfg.add_cpu_to_node(n, c))
            std::exit(EXIT_FAILURE);
        } else if (parts.size() == 2) {
          const CpuIndex cfirst = CpuIndex{std::stoll(parts[0])};
          const CpuIndex clast = CpuIndex{std::stoll(parts[1])};
          if (!cfg.add_cpu_range_to_node(n, cfirst, clast))
            std::exit(EXIT_FAILURE);
        } else {
          std::exit(EXIT_FAILURE);
        }
      }
      n += 1;
    }

    return cfg;
  }

  NumaConfig(const NumaConfig&) = delete;
  NumaConfig(NumaConfig&&) = default;
  NumaConfig& operator=(const NumaConfig&) = delete;
  NumaConfig& operator=(NumaConfig&&) = default;

  bool is_cpu_assigned(CpuIndex n) const {
    return nodeByCpu.count(n) == 1;
  }

  NumaIndex num_numa_nodes() const {
    return nodes.size();
  }

  CpuIndex num_cpus() const {
    return nodeByCpu.size(); 
  }

private:
  std::vector<std::set<CpuIndex>> nodes;
  std::map<CpuIndex, NumaIndex> nodeByCpu;

  struct EmptyNodeTag {};

  NumaConfig(EmptyNodeTag) {}

  // Returns true if successful
  // Returns false if failed, i.e. when the cpu is already present
  //                          strong guarantee, the structure remains unmodified
  bool add_cpu_to_node(NumaIndex n, CpuIndex c) {
    if (is_cpu_assigned(c))
      return false;

    while (nodes.size() < n) {
      nodes.emplace_back();
    }

    nodes[n].insert(c);
    nodeByCpu[c] = n;

    return true;
  }

  // Returns true if successful
  // Returns false if failed, i.e. when any of the cpus is already present
  //                          strong guarantee, the structure remains unmodified
  bool add_cpu_range_to_node(NumaIndex n, CpuIndex cfirst, CpuIndex clast) {
    for (CpuIndex c = cfirst; c <= clast; ++c)
      if (is_cpu_assigned(c))
        return false;

    while (nodes.size() < n) {
      nodes.emplace_back();
    }

    for (CpuIndex c = cfirst; c <= clast; ++c) {
      nodes[n].insert(c);
      nodeByCpu[c] = n;
    }

    return true;
  }
};

class NumaReplicationContext;

// Instances of this class are tracked by the context
class NumaReplicatedBase {
public:
  NumaReplicatedBase(NumaReplicationContext& ctx) :
    context(&ctx),
    uniqueId(getNextUniqueId()) 
  {

  }

  NumaReplicatedBase(const NumaReplicatedBase&) = delete;
  NumaReplicatedBase(NumaReplicatedBase&& other);

  NumaReplicatedBase& operator=(const NumaReplicatedBase&) = delete;
  NumaReplicatedBase& operator=(NumaReplicatedBase&& other);

  virtual void on_numa_config_changed(UpdateNumaConfigPermission) = 0;
  virtual ~NumaReplicatedBase();

  size_t get_unique_id() const {
    return uniqueId;
  }

private:
  NumaReplicationContext* context;
  size_t uniqueId;

  static size_t InvalidUniqueId = size_t(-1);

  static size_t getNextUniqueId() {
    static std::atomic<size_t> counter{0};
    return counter.fetch_add(1);
  }
};

// We force boxing with a unique_ptr. If this becomes an issue due to added indirection we
// may need to add an option for a custom boxing type.
// When the NUMA config changes the value stored at the index 0 is replicated to other nodes.
template <typename T>
class NumaReplicated : public NumaReplicatedBase {
public:
  using ReplicatorFuncType = std::function<void(T&, const T&)>;

  NumaReplicated() :
    NumaReplicatedBase(),
    replicatorFunc([](T& destination, const T& source) { destination = source; }) 
  {
    instances.emplace_back(std::make_unique<T>());
  }

  template <typename FuncT>
  NumaReplicated(FuncT&& func) :
    NumaReplicatedBase(),
    replicatorFunc(std::forward<FuncT>(func)) 
  {
    instances.emplace_back(std::make_unique<T>());
  }

  NumaReplicated(const NumaReplicated&) = delete;
  NumaReplicated(NumaReplicated&& other) noexcept : 
    NumaReplicatedBase(std::move(other)),
    replicatorFunc(std::exchange(other.replicatorFunc, nullptr)),
    instances(std::exchange(other.instances, {}))
  {

  }

  NumaReplicated& operator=(const NumaReplicated&) = delete;
  NumaReplicated& operator=(NumaReplicated&& other) noexcept {
    NumaReplicatedBase::operator=(*this, std::move(other));
    replicatorFunc = std::exchange(other.replicatorFunc, nullptr);
    instances = std::exchange(other.instances, {});
  }

  ~NumaReplicated() override = default;

  NumaReplicated(CreateObjectPermission) {}

  void on_numa_config_changed() override {
    // TODO: replicate
  }

private:
  ReplicatorFuncType replicatorFunc;
  std::vector<std::unique_ptr<T>> instances;
};

class NumaReplicationContext {

  NumaReplicationContext(const NumaReplicationContext&) = delete;
  NumaReplicationContext(NumaReplicationContext&&) = delete;

  NumaReplicationContext& operator=(const NumaReplicationContext&) = delete;
  NumaReplicationContext& operator=(NumaReplicationContext&&) = delete;

  ~NumaReplicationContext() {
    // The context must outlive replicated objects
    if (!trackedReplicatedObjects.empty())
      std::exit(EXIT_FAILURE);
  }

  void detach(NumaReplicatedBase* obj) {
    assert(trackedReplicatedObjects.count(obj->get_unique_id()) == 1);
    assert(trackedReplicatedObjects[obj->get_unique_id()] == obj);
    trackedReplicatedObjects.erase(obj->get_unique_id());
  }

  // oldObj may be invalid at this point
  void move_attached(NumaReplicatedBase* oldObj, NumaReplicatedBase* newObj) {
    assert(trackedReplicatedObjects.count(newObj->get_unique_id()) == 1);
    assert(trackedReplicatedObjects[newObj->get_unique_id()] == oldObj);
    trackedReplicatedObjects[newObj->get_unique_id()] = newObj;
  }

private:
  std::map<size_t, NumaReplicatedBase*> trackedReplicatedObjects;
};


NumaReplicatedBase::NumaReplicatedBase(NumaReplicatedBase&& other) noexcept : 
  context(std::exchange(other.context, nullptr)),
  uniqueId(std::exchange(other.uniqueId, InvalidUniqueId))
{
  context->move_attached(&other, this);
}

NumaReplicatedBase& NumaReplicatedBase::operator=(NumaReplicatedBase&& other) noexcept
{
  context = std::exchange(other.context, nullptr);
  uniqueId = std::exchange(other.uniqueId, InvalidUniqueId);

  context->move_attached(&other, this);
}

NumaReplicatedBase::~NumaReplicatedBase() {
  context->detach(this);
}

}  // namespace Stockfish


#endif  // #ifndef NUMA_H_INCLUDED
