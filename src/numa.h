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
#include <sstream>

#if defined(__linux__)
# if !defined(_GNU_SOURCE)
#   define _GNU_SOURCE
# endif
# include <sched.h>
#elif defined(_WIN32)
# if !defined(NOMINMAX)
#   define NOMINMAX
# endif
# include <windows.h>

using SetThreadSelectedCpuSetMasks_t
                  = bool (*)(HANDLE,
                    PGROUP_AFFINITY,
                    USHORT);

using SetThreadGroupAffinity_t
                  = bool (*)(HANDLE, 
                    const GROUP_AFFINITY, 
                    PGROUP_AFFINITY);

#endif

#include "misc.h"

#if defined(_WIN32)

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

#endif

inline int get_current_cpu() {

#if defined(__linux__)
  return sched_getcpu();
#elif defined(_WIN32)
  return GetCurrentProcessorNumber();
#endif

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

    // On linux things are straightforward, since there's no processor groups and
    // any thread can be scheduled on all processors.
    std::istringstream ss(get_system_command_output("lscpu -e=cpu,node"));

    // skip the list header
    ss.ignore(std::numeric_limits<std::streamsize>::max(), '\n');

    for(;;) {
      CpuIndex c;
      NumaIndex n;

      ss >> c >> n;

      if (!ss)
        break;

      cfg.add_cpu_to_node(n, c);
    }

#elif defined(_WIN32)

    // Since Windows 11 and Windows Server 2022 thread affinities can span
    // processor groups and can be set as such by a new WinAPI function.
    static const bool CanAffinitySpanProcessorGroups = []() {
      HMODULE k32  = GetModuleHandle(TEXT("Kernel32.dll"));
      auto    SetThreadSelectedCpuSetMasks_f = SetThreadSelectedCpuSetMasks_t((void (*)()) GetProcAddress(k32, "SetThreadSelectedCpuSetMasks"));
      return SetThreadSelectedCpuSetMasks_f != nullptr;
    }();
  
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

    // Split the NUMA nodes to be contained within a group if necessary.
    // This is needed between Windows 10 Build 20348 and Windows 11, because
    // the new NUMA allocation behaviour was introduced while there was
    // still no way to set thread affinity spanning multiple processor groups.
    // See https://learn.microsoft.com/en-us/windows/win32/procthread/numa-support
    if (!CanAffinitySpanProcessorGroups) {
      NumaConfig splitCfg = empty();

      NumaIndex splitNodeIndex = 0;
      for (const auto& cpus : nodes) {
        if (cpus.empty())
          continue;

        size_t lastProcGroupIndex = cpus[0] / 64;
        for (CpuIndex c : cpus) {
          const size_t procGroupIndex = c / 64;
          if (procGroupIndex != lastProcGroupIndex) {
            splitNodeIndex += 1;
            lastProcGroupIndex = procGroupIndex;
          }
          splitCfg.add_cpu_to_node(splitNodeIndex, c);
        }
        splitNodeIndex += 1;
      }

      cfg = std::move(splitCfg);
    }

#else

    // Fallback for unsupported systems.
    const CpuIndex numCpus = CpuIndex{std::max<CpuIndex>(1, std::thread::hardware_concurrency())};
    cfg.add_cpu_range_to_node(NumaIndex{0}, CpuIndex{0}, numCpus-1);

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
          // NOTE: Do we want to perhaps fill the first available node up to 50% first before considering other nodes?
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

    const size_t masksize = CPU_ALLOC_SIZE(highestCpuIndex + 1);

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
    HMODULE k32  = GetModuleHandle(TEXT("Kernel32.dll"));
    auto    SetThreadSelectedCpuSetMasks_f = SetThreadSelectedCpuSetMasks_t((void (*)()) GetProcAddress(k32, "SetThreadSelectedCpuSetMasks"));
    auto    SetThreadGroupAffinity_f = SetThreadGroupAffinity_t((void (*)()) GetProcAddress(k32, "SetThreadGroupAffinity"));

    if (SetThreadSelectedCpuSetMasks_f != nullptr) {
      // Only available on Windows 11 and Windows Server 2022 onwards.
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
    } else if (SetThreadGroupAffinity_f != nullptr) {
      // On earlier windows version (since windows 7) we can't run a single thread
      // on multiple processor groups, so we need to restrict the group.
      // We assume the group of the first processor listed for this node.
      // Processors from outside this group will not be assigned for this thread.
      // Normally this won't be an issue because windows used to assign NUMA nodes
      // such that they can't span processor groups. However, since Windows 10 Build 20348
      // the behaviour changed, so there's a small window of versions between this and Windows 11
      // that might exhibit problems with not all processors being utilized. 
      // We handle this in NumaConfig::from_system by manually splitting the nodes when
      // we detect that there's no function to set affinity spanning processor nodes.
      // This is required because otherwise our thread distribution code may produce
      // suboptimal results.
      // See https://learn.microsoft.com/en-us/windows/win32/procthread/numa-support
      GROUP_AFFINITY affinity;
      std::memset(&affinity, 0, sizeof(GROUP_AFFINITY));
      affinity.Group = static_cast<WORD>(n);
      const size_t forcedProcGroupIndex = nodes[n][0] / 64;
      for (CpuIndex c : nodes[n]) {
        const size_t procGroupIndex = c / 64;
        const size_t idxWithinProcGroup = c % 64;
        // We skip processors that are not in the same proccessor group.
        // If everything was set up correctly this will never be an issue,
        // but we have to account for bad NUMA node specification.
        if (procGroupIndex != forcedProcGroupIndex)
          continue;

        affinity.Mask |= KAFFINITY(1) << idxWithinProcGroup;
      }

      HANDLE hThread = GetCurrentThread();

      const BOOL status = SetThreadGroupAffinity_f(hThread, &affinity, nullptr);
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
      std::cout << "before bind on node " << n << ": " << get_current_cpu() << '\n';
      bind_current_thread_to_numa_node(n);
      std::cout << "after bind: " << get_current_cpu() << '\n';
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
