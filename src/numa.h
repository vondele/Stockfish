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

#include <vector>
#include <set>
#include <map>
#include <thread>

namespace Stockfish {

class Numa {
public:
  using CpuIndex = size_t;
  using NumaIndex = size_t;

  Numa() {
    numCpus = std::max(1, std::thread::hardware_concurrency());
    addCpuRangeToDomain(0, 0, numCpus-1);
  }

  Numa(const Numa&) = delete;
  Numa(Numa&&) = default;
  Numa& operator=(const Numa&) = delete;
  Numa& operator=(Numa&&) = default;

  bool hasCpuAssignedDomain(CpuIndex n) const {
    return cpuToNumaMapping.count(n) == 1;
  }

private:
  CpuIndex numCpus;
  std::vector<std::set<CpuIndex>> cpuDomains;
  std::map<CpuIndex, NumaIndex> cpuToNumaMapping;

  // Returns true if successful
  // Returns false if failed, i.e. when the cpu is already present
  //                          strong guarantee, the structure remains unmodified
  bool addCpuToDomain(NumaIndex n, CpuIndex c) {
    if (hasCpuAssignedDomain(c))
      return false;

    while (cpuDomains.size() < n) {
      cpuDomains.emplace_back();
    }

    cpuDomains[n].insert(c);
    cpuToNumaMapping[c] = n;

    return true;
  }

  // Returns true if successful
  // Returns false if failed, i.e. when any of the cpus is already present
  //                          strong guarantee, the structure remains unmodified
  bool addCpuRangeToDomain(NumaIndex n, CpuIndex cfirst, CpuIndex clast) {
    for (CpuIndex c = cfirst; c <= clast; ++c)
      if (hasCpuAssignedDomain(c))
        return false;

    while (cpuDomains.size() < n) {
      cpuDomains.emplace_back();
    }

    for (CpuIndex c = cfirst; c <= clast; ++c) {
      cpuDomains[n].insert(c);
      cpuToNumaMapping[c] = n;
    }

    return true;
  }
};

}  // namespace Stockfish


#endif  // #ifndef NUMA_H_INCLUDED
