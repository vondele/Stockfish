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
#include <vector>
#include <set>
#include <map>
#include <thread>
#include <string>
#include <memory>

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
    addCpuRangeToNode(NumaIndex{0}, CpuIndex{0}, numCpus-1);
  }

  static NumaConfig empty() {
    return NumaConfig(EmptyNodeTag{});
  }

  static NumaConfig fromSystem() {
    // TODO: winapi/lscpu
  }

  // ':'-separated numa nodes
  // ','-separated cpu indices
  // supports "first-last" range syntax for cpu indices, 
  static NumaConfig fromString(const std::string& s) {
    NumaConfig cfg = empty();

    NumaIndex n = 0;
    for (auto&& nodeStr : split(s, ":")) {
      for (const std::string& cpuStr : split(nodeStr, ",")) {
        if (cpuStr.empty())
          continue

        auto parts = split(cpuStr, "-");
        if (parts.size() == 1) {
          const CpuIndex c = CpuIndex{std::stoll(parts[0])};
          if (!cfg.addCpuToNode(n, c))
            std::exit(EXIT_FAILURE);
        } else if (parts.size() == 2) {
          const CpuIndex cfirst = CpuIndex{std::stoll(parts[0])};
          const CpuIndex clast = CpuIndex{std::stoll(parts[1])};
          if (!cfg.addCpuRangeToNode(n, cfirst, clast))
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

  bool isCpuAssigned(CpuIndex n) const {
    return nodeByCpu.count(n) == 1;
  }

  NumaIndex numNumaNodes() const {
    return nodes.size();
  }

  CpuIndex numCpus() const {
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
  bool addCpuToNode(NumaIndex n, CpuIndex c) {
    if (isCpuAssigned(c))
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
  bool addCpuRangeToNode(NumaIndex n, CpuIndex cfirst, CpuIndex clast) {
    for (CpuIndex c = cfirst; c <= clast; ++c)
      if (isCpuAssigned(c))
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

class NumaReplicatedBase {
public:
  struct UpdateNumaConfigPermission {
    friend class NumaReplicationContext;
  private:
    UpdateNumaConfigPermission() {}
  };

  struct CreateObjectPermission {
    friend class NumaReplicationContext;
  private:
    CreateObjectPermission() {}
  };

  virtual void onNumaConfigChanged(UpdateNumaConfigPermission, const NumaConfig& cfg) = 0;
  virtual ~NumaReplicatedBase() = default;
};

// We force boxing with a unique_ptr. If this becomes an issue due to added indirection we
// may need to add an option for a custom boxing type.
// When the NUMA config changes the value stored at the index 0 is replicated to other nodes.
template <typename T>
class NumaReplicated : public NumaReplicatedBase{
  static_assert(std::is_copy_constructible_v<T>);
public:
  NumaReplicated() = delete;

  NumaReplicated(const NumaReplicated&) = delete;
  NumaReplicated(NumaReplicated&&) = delete;

  NumaReplicated& operator=(const NumaReplicated&) = delete;
  NumaReplicated& operator=(NumaReplicated&&) = delete;

  ~NumaReplicated() override = default;

  NumaReplicated(CreateObjectPermission) {}

  void onNumaConfigChanged(UpdateNumaConfigPermission, const NumaConfig& cfg) override {
    // TODO: replicate
  }

private:
  std::vector<std::unique_ptr<T>> instances;
};

class NumaReplicationContext {

};

}  // namespace Stockfish


#endif  // #ifndef NUMA_H_INCLUDED
