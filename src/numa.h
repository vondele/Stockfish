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
#include <limits>
#include <iostream>

#if defined(__linux__)
# include <sched.h>
#elif defined(_WIN32)
# if !defined(NOMINMAX)
#   define NOMINMAX
# endif
# include <windows.h>
#endif

#include "misc.h"

inline std::string GetLastErrorAsString()
{
    //Get the error message ID, if any.
    DWORD errorMessageID = ::GetLastError();
    if(errorMessageID == 0) {
        return std::string("No recorded error"); //No error message has been recorded
    }
    
    LPSTR messageBuffer = nullptr;

    //Ask Win32 to give us the string version of that message ID.
    //The parameters we pass in, tell Win32 to create the buffer that holds the message for us (because we don't yet know how long the message string will be).
    size_t size = FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                                 NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&messageBuffer, 0, NULL);
    
    //Copy the error message into a std::string.
    std::string message(messageBuffer, size);
    
    //Free the Win32's string's buffer.
    LocalFree(messageBuffer);
            
    return message;
}

namespace Stockfish {

using CpuIndex = size_t;
using NumaIndex = size_t;

// Designed as immutable, because there is no good reason to alter an already existing config
// in a way that doesn't require recreating it completely.
class NumaConfig {
public:
  NumaConfig() :
    highestCpuIndex(0)
  {
    const CpuIndex numCpus = CpuIndex{std::max<CpuIndex>(1, std::thread::hardware_concurrency())};
    std::cout << "creating numa config with " << numCpus << " cpus\n";
    add_cpu_range_to_node(NumaIndex{0}, CpuIndex{0}, numCpus-1);
    std::cout << "created numa config\n";
  }

  static NumaConfig empty() {
    return NumaConfig(EmptyNodeTag{});
  }

  static NumaConfig from_system() {
    NumaConfig cfg = empty();

#if defined(__linux__)

    const std::istringstream ss = get_system_command_output("lscpu -e=cpu,node");

    // skip the list header
    ss.ignore('\n');

    for(;;) {
      CpuIndex c;
      NumaIndex n;

      ss >> c >> n;

      if (ss) {
        cfg.add_cpu_to_node(n, c);
      }
    }

#elif defined(_WIN32)

    WORD numProcGroups = GetActiveProcessorGroupCount();
    for (WORD procGroup = 0; procGroup < numProcGroups; ++procGroup) {
      for (BYTE number = 0; number < 64; ++number) {
        PROCESSOR_NUMBER procnum;
        procnum.Group = procGroup;
        procnum.Number = number;
        procnum.Reserved = 0;
        USHORT nodeNumber;
        const BOOL status = GetNumaProcessorNodeEx(&procnum, &nodeNumber);
        if (status != 0 && nodeNumber != std::numeric_limits<USHORT>::max()) {
          cfg.add_cpu_to_node(nodeNumber, static_cast<CpuIndex>(procGroup) * 64 + static_cast<CpuIndex>(number));
        }
      }
    }
    
#endif

    return cfg;
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
          continue;

        auto parts = split(cpuStr, "-");
        if (parts.size() == 1) {
          const CpuIndex c = CpuIndex{std::stoull(parts[0])};
          if (!cfg.add_cpu_to_node(n, c))
            std::exit(EXIT_FAILURE);
        } else if (parts.size() == 2) {
          const CpuIndex cfirst = CpuIndex{std::stoull(parts[0])};
          const CpuIndex clast = CpuIndex{std::stoull(parts[1])};
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

  std::vector<NumaIndex> distribute_threads_among_numa_nodes(CpuIndex numThreads) const {
    std::vector<NumaIndex> ns;

    if (nodes.size() == 1) {
      // special case for when there's no NUMA nodes
      // doesn't buy us much, but let's keep the default path simple
      ns.resize(numThreads, NumaIndex{0});
    } else {
      std::vector<size_t> occupation(nodes.size(), 0);
      for (CpuIndex c = 0; c < numThreads; ++c) {
        NumaIndex bestNode{0};
        float bestNodeFill = std::numeric_limits<float>::max();
        for (NumaIndex n = 0; n < nodes.size(); ++n) {
          float fill = static_cast<float>(occupation[n] + 1) / static_cast<float>(nodes[n].size());
          if (fill < bestNodeFill) {
            bestNode = n;
            bestNodeFill = fill;
          }
        }
        ns.emplace_back(bestNode);
        occupation[bestNode] += 1;
      }
    }

    return ns;
  }

  void bind_current_thread_to_numa_node(NumaIndex n) const {
    if (n >= nodes.size() || nodes[n].size() == 0)
      std::exit(EXIT_FAILURE);

#if defined(__linux__)

    cpu_set_t* mask = CPU_ALLOC(highestCpuIndex + 1);
    if (mask == nullptr)
        exit(EXIT_FAILURE);

    const size_t masksize = CPU_ALLOC_SIZE(num_cpus);

    CPU_ZERO_S(masksize, mask);
    
    for (CpuIndex c : nodes[n])
      CPU_SET_S(c, masksize, mask);
    
    const int status = sched_setaffinity(0, masksize, mask);

    CPU_FREE(mask);

    if (status != 0)
      std::exit(EXIT_FAILURE);

    // Might not be necessary, might not be enough, we'll see.
    sched_yield();

#elif defined(_WIN32)

    // Requires Windows 11. No good way to set thread affinity spanning processor groups before that.
    using SetThreadSelectedCpuSetMasks_t
                      = bool (*)(HANDLE,
                        PGROUP_AFFINITY,
                        USHORT);
    HMODULE k32  = GetModuleHandle(TEXT("Kernel32.dll"));
    auto    SetThreadSelectedCpuSetMasks_f = SetThreadSelectedCpuSetMasks_t((void (*)()) GetProcAddress(k32, "SetThreadSelectedCpuSetMasks"));

    if (SetThreadSelectedCpuSetMasks_f != nullptr) {
      const USHORT numProcGroups = ((highestCpuIndex + 1) + 63) / 64;
      auto groupAffinities = std::make_unique<GROUP_AFFINITY[]>(numProcGroups);
      std::memset(groupAffinities.get(), 0, sizeof(GROUP_AFFINITY) * numProcGroups);
      for (WORD i = 0; i < numProcGroups; ++i)
        groupAffinities[i].Group = i;

      for (CpuIndex c : nodes[n]) {
        const size_t procGroupIndex = c / 64;
        const size_t idxWithinProcGroup = c % 64;
        groupAffinities[procGroupIndex].Mask |= KAFFINITY(1) << idxWithinProcGroup;
      }

      std::cout << "highest cpu index: " << highestCpuIndex << '\n';
      std::cout << "num proc groups: " << numProcGroups << '\n';
      for (int i = 0; i < numProcGroups; ++i) {
        std::cout << "\t" << groupAffinities[i].Mask << '\n';
      }

      HANDLE hThread = GetCurrentThread();

      const BOOL status = SetThreadSelectedCpuSetMasks_f(hThread, groupAffinities.get(), numProcGroups);
      if (status == 0) {
        auto err = GetLastErrorAsString();
        std::cerr << "ERR: " << err << '\n';
        std::exit(EXIT_FAILURE);
      }

      // Might not be necessary, might not be enough, we'll see.
      SwitchToThread();
    }

#endif
  }

  template <typename FuncT>
  void execute_on_numa_node(NumaIndex n, FuncT&& f) const {
    std::thread th([this, &f, n](){
      std::cout << "before bind on node " << n << ": " << GetCurrentProcessorNumber() << '\n';
      bind_current_thread_to_numa_node(n);
      std::cout << "after bind: " << GetCurrentProcessorNumber() << '\n';
      std::forward<FuncT>(f)();
    });

    th.join();
  }

private:
  std::vector<std::set<CpuIndex>> nodes;
  std::map<CpuIndex, NumaIndex> nodeByCpu;
  CpuIndex highestCpuIndex;

  struct EmptyNodeTag {};

  NumaConfig(EmptyNodeTag) :
    highestCpuIndex(0)
  {

  }

  // Returns true if successful
  // Returns false if failed, i.e. when the cpu is already present
  //                          strong guarantee, the structure remains unmodified
  bool add_cpu_to_node(NumaIndex n, CpuIndex c) {
    std::cout << "adding cpu " << c << " to node " << n << '\n';
    if (is_cpu_assigned(c))
      return false;

    while (nodes.size() <= n) {
      nodes.emplace_back();
    }

    nodes[n].insert(c);
    nodeByCpu[c] = n;

    if (c > highestCpuIndex)
      highestCpuIndex = c;

    return true;
  }

  // Returns true if successful
  // Returns false if failed, i.e. when any of the cpus is already present
  //                          strong guarantee, the structure remains unmodified
  bool add_cpu_range_to_node(NumaIndex n, CpuIndex cfirst, CpuIndex clast) {
    std::cout << "adding cpu range " << cfirst << "-" << clast << " to node " << n << '\n';
    for (CpuIndex c = cfirst; c <= clast; ++c)
      if (is_cpu_assigned(c))
        return false;

    while (nodes.size() <= n) {
      nodes.emplace_back();
    }

    for (CpuIndex c = cfirst; c <= clast; ++c) {
      nodes[n].insert(c);
      nodeByCpu[c] = n;
    }

    if (clast > highestCpuIndex)
      highestCpuIndex = clast;

    return true;
  }
};

class NumaReplicationContext;

// Instances of this class are tracked by the context
class NumaReplicatedBase {
public:
  NumaReplicatedBase(NumaReplicationContext& ctx);

  NumaReplicatedBase(const NumaReplicatedBase&) = delete;
  NumaReplicatedBase(NumaReplicatedBase&& other) noexcept;

  NumaReplicatedBase& operator=(const NumaReplicatedBase&) = delete;
  NumaReplicatedBase& operator=(NumaReplicatedBase&& other) noexcept;

  virtual void on_numa_config_changed() = 0;
  virtual ~NumaReplicatedBase();

  size_t get_unique_id() const {
    return uniqueId;
  }

  const NumaConfig& get_numa_config() const;

private:
  NumaReplicationContext* context;
  size_t uniqueId;

  static constexpr size_t InvalidUniqueId = size_t(-1);

  static size_t get_next_unique_id() {
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
  using ReplicatorFuncType = std::function<T(const T&)>;

  NumaReplicated(NumaReplicationContext& ctx) :
    NumaReplicatedBase(ctx),
    replicatorFunc([](const T& src) -> T { return src; }) 
  {
    replicate_from(T{});
  }

  NumaReplicated(NumaReplicationContext& ctx, const T& source) :
    NumaReplicatedBase(ctx),
    replicatorFunc([](const T& src) -> T { return src; }) 
  {
    replicate_from(source);
  }

  template <typename FuncT>
  NumaReplicated(NumaReplicationContext& ctx, FuncT&& func) :
    NumaReplicatedBase(ctx),
    replicatorFunc(std::forward<FuncT>(func)) 
  {
    replicate_from(T{});
  }

  template <typename FuncT>
  NumaReplicated(NumaReplicationContext& ctx, const T& source, FuncT&& func) :
    NumaReplicatedBase(ctx),
    replicatorFunc(std::forward<FuncT>(func)) 
  {
    replicate_from(source);
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

    return *this;
  }

  NumaReplicated& operator=(const T& source) {
    replicate_from(source);

    return *this;
  }

  ~NumaReplicated() override = default;

  NumaReplicated() {}

  void on_numa_config_changed() override {
    // Use the first one as the source. It doesn't matter which one we use, because they all must
    // be identical, but the first one is guaranteed to exist.
    auto source = std::move(instances[0]);
    replicate_from(*source);
  }

private:
  ReplicatorFuncType replicatorFunc;
  std::vector<std::unique_ptr<T>> instances;

  void replicate_from(const T& source) {
    std::cout << "Replicating...\n";

    instances.clear();

    const NumaConfig& cfg = get_numa_config();
    for (NumaIndex n = 0; n < cfg.num_numa_nodes(); ++n) {
      cfg.execute_on_numa_node(n, [this, &source](){ instances.emplace_back(std::make_unique<T>(replicatorFunc(source))); });
    }
  }
};

class NumaReplicationContext {
public:
  NumaReplicationContext() {}

  NumaReplicationContext(const NumaReplicationContext&) = delete;
  NumaReplicationContext(NumaReplicationContext&&) = delete;

  NumaReplicationContext& operator=(const NumaReplicationContext&) = delete;
  NumaReplicationContext& operator=(NumaReplicationContext&&) = delete;

  ~NumaReplicationContext() {
    // The context must outlive replicated objects
    if (!trackedReplicatedObjects.empty())
      std::exit(EXIT_FAILURE);
  }

  void attach(NumaReplicatedBase* obj) {
    assert(trackedReplicatedObjects.count(obj->get_unique_id()) == 0);
    trackedReplicatedObjects.try_emplace(obj->get_unique_id(), obj);
  }

  void detach(NumaReplicatedBase* obj) {
    std::cout << "detach " << obj->get_unique_id() << '\n';
    assert(trackedReplicatedObjects.count(obj->get_unique_id()) == 1);
    assert(trackedReplicatedObjects[obj->get_unique_id()] == obj);
    trackedReplicatedObjects.erase(obj->get_unique_id());
  }

  // oldObj may be invalid at this point
  void move_attached([[maybe_unused]] NumaReplicatedBase* oldObj, NumaReplicatedBase* newObj) {
    std::cout << "move " << newObj->get_unique_id() << '\n';
    assert(trackedReplicatedObjects.count(newObj->get_unique_id()) == 1);
    assert(trackedReplicatedObjects[newObj->get_unique_id()] == oldObj);
    trackedReplicatedObjects[newObj->get_unique_id()] = newObj;
  }

  void set_numa_config(NumaConfig&& cfg) {
    config = std::move(cfg);
    for (auto&& [id, obj] : trackedReplicatedObjects)
      obj->on_numa_config_changed();
  }

  const NumaConfig& get_numa_config() const {
    return config;
  }

private:
  NumaConfig config;
  std::map<size_t, NumaReplicatedBase*> trackedReplicatedObjects;
};

NumaReplicatedBase::NumaReplicatedBase(NumaReplicationContext& ctx) :
  context(&ctx),
  uniqueId(get_next_unique_id()) 
{
  context->attach(this);
}

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

  return *this;
}

NumaReplicatedBase::~NumaReplicatedBase() {
  if (context != nullptr)
    context->detach(this);
}

const NumaConfig& NumaReplicatedBase::get_numa_config() const {
  return context->get_numa_config();
}

}  // namespace Stockfish


#endif  // #ifndef NUMA_H_INCLUDED
