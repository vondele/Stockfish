/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2025 The Stockfish developers (see AUTHORS file)

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

#ifndef MEMORY_H_INCLUDED
#define MEMORY_H_INCLUDED

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <fcntl.h>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <memory>
#include <new>
#include <string>
#include <sys/mman.h>
#include <sys/stat.h>
#include <type_traits>
#include <unistd.h>
#include <utility>
#include <vector>

#include "types.h"

namespace Stockfish {

void* std_aligned_alloc(size_t alignment, size_t size);
void  std_aligned_free(void* ptr);

// Memory aligned by page size, min alignment: 4096 bytes
void* aligned_large_pages_alloc(size_t size);
void  aligned_large_pages_free(void* mem);

bool has_large_pages();

// Frees memory which was placed there with placement new.
// Works for both single objects and arrays of unknown bound.
template<typename T, typename FREE_FUNC>
void memory_deleter(T* ptr, FREE_FUNC free_func) {
    if (!ptr)
        return;

    // Explicitly needed to call the destructor
    if constexpr (!std::is_trivially_destructible_v<T>)
        ptr->~T();

    free_func(ptr);
}

// Frees memory which was placed there with placement new.
// Works for both single objects and arrays of unknown bound.
template<typename T, typename FREE_FUNC>
void memory_deleter_array(T* ptr, FREE_FUNC free_func) {
    if (!ptr)
        return;


    // Move back on the pointer to where the size is allocated
    const size_t array_offset = std::max(sizeof(size_t), alignof(T));
    char*        raw_memory   = reinterpret_cast<char*>(ptr) - array_offset;

    if constexpr (!std::is_trivially_destructible_v<T>)
    {
        const size_t size = *reinterpret_cast<size_t*>(raw_memory);

        // Explicitly call the destructor for each element in reverse order
        for (size_t i = size; i-- > 0;)
            ptr[i].~T();
    }

    free_func(raw_memory);
}

// Allocates memory for a single object and places it there with placement new
template<typename T, typename ALLOC_FUNC, typename... Args>
inline std::enable_if_t<!std::is_array_v<T>, T*> memory_allocator(ALLOC_FUNC alloc_func,
                                                                  Args&&... args) {
    void* raw_memory = alloc_func(sizeof(T));
    ASSERT_ALIGNED(raw_memory, alignof(T));
    return new (raw_memory) T(std::forward<Args>(args)...);
}

// Allocates memory for an array of unknown bound and places it there with placement new
template<typename T, typename ALLOC_FUNC>
inline std::enable_if_t<std::is_array_v<T>, std::remove_extent_t<T>*>
memory_allocator(ALLOC_FUNC alloc_func, size_t num) {
    using ElementType = std::remove_extent_t<T>;

    const size_t array_offset = std::max(sizeof(size_t), alignof(ElementType));

    // Save the array size in the memory location
    char* raw_memory =
      reinterpret_cast<char*>(alloc_func(array_offset + num * sizeof(ElementType)));
    ASSERT_ALIGNED(raw_memory, alignof(T));

    new (raw_memory) size_t(num);

    for (size_t i = 0; i < num; ++i)
        new (raw_memory + array_offset + i * sizeof(ElementType)) ElementType();

    // Need to return the pointer at the start of the array so that
    // the indexing in unique_ptr<T[]> works.
    return reinterpret_cast<ElementType*>(raw_memory + array_offset);
}

//
//
// aligned large page unique ptr
//
//

template<typename T>
struct LargePageDeleter {
    void operator()(T* ptr) const { return memory_deleter<T>(ptr, aligned_large_pages_free); }
};

template<typename T>
struct LargePageArrayDeleter {
    void operator()(T* ptr) const { return memory_deleter_array<T>(ptr, aligned_large_pages_free); }
};

template<typename T>
using LargePagePtr =
  std::conditional_t<std::is_array_v<T>,
                     std::unique_ptr<T, LargePageArrayDeleter<std::remove_extent_t<T>>>,
                     std::unique_ptr<T, LargePageDeleter<T>>>;

// make_unique_large_page for single objects
template<typename T, typename... Args>
std::enable_if_t<!std::is_array_v<T>, LargePagePtr<T>> make_unique_large_page(Args&&... args) {
    static_assert(alignof(T) <= 4096,
                  "aligned_large_pages_alloc() may fail for such a big alignment requirement of T");

    T* obj = memory_allocator<T>(aligned_large_pages_alloc, std::forward<Args>(args)...);

    return LargePagePtr<T>(obj);
}

// make_unique_large_page for arrays of unknown bound
template<typename T>
std::enable_if_t<std::is_array_v<T>, LargePagePtr<T>> make_unique_large_page(size_t num) {
    using ElementType = std::remove_extent_t<T>;

    static_assert(alignof(ElementType) <= 4096,
                  "aligned_large_pages_alloc() may fail for such a big alignment requirement of T");

    ElementType* memory = memory_allocator<T>(aligned_large_pages_alloc, num);

    return LargePagePtr<T>(memory);
}

//
//
// aligned unique ptr
//
//

template<typename T>
struct AlignedDeleter {
    void operator()(T* ptr) const { return memory_deleter<T>(ptr, std_aligned_free); }
};

template<typename T>
struct AlignedArrayDeleter {
    void operator()(T* ptr) const { return memory_deleter_array<T>(ptr, std_aligned_free); }
};

template<typename T>
using AlignedPtr =
  std::conditional_t<std::is_array_v<T>,
                     std::unique_ptr<T, AlignedArrayDeleter<std::remove_extent_t<T>>>,
                     std::unique_ptr<T, AlignedDeleter<T>>>;

// make_unique_aligned for single objects
template<typename T, typename... Args>
std::enable_if_t<!std::is_array_v<T>, AlignedPtr<T>> make_unique_aligned(Args&&... args) {
    const auto func = [](size_t size) { return std_aligned_alloc(alignof(T), size); };
    T*         obj  = memory_allocator<T>(func, std::forward<Args>(args)...);

    return AlignedPtr<T>(obj);
}

// make_unique_aligned for arrays of unknown bound
template<typename T>
std::enable_if_t<std::is_array_v<T>, AlignedPtr<T>> make_unique_aligned(size_t num) {
    using ElementType = std::remove_extent_t<T>;

    const auto   func   = [](size_t size) { return std_aligned_alloc(alignof(ElementType), size); };
    ElementType* memory = memory_allocator<T>(func, num);

    return AlignedPtr<T>(memory);
}


// Get the first aligned element of an array.
// ptr must point to an array of size at least `sizeof(T) * N + alignment` bytes,
// where N is the number of elements in the array.
template<uintptr_t Alignment, typename T>
T* align_ptr_up(T* ptr) {
    static_assert(alignof(T) < Alignment);

    const uintptr_t ptrint = reinterpret_cast<uintptr_t>(reinterpret_cast<char*>(ptr));
    return reinterpret_cast<T*>(
      reinterpret_cast<char*>((ptrint + (Alignment - 1)) / Alignment * Alignment));
}


namespace fs = std::filesystem;

template<typename T>
class SharedMemoryManager {
   private:
    std::string name;
    size_t      elementCount;
    T*          mmapAddr;
    int         fd;
    size_t      mmapSize;
    bool        isOwner;
    bool        isInit;

    static std::string build_shm_name(const std::string& username, const std::string& shaVersion) {
        std::string baseSegment = username + "_sf-shared-net_";

        // Add NUMA node info
        char numaBuf[64] = {0};
        int  numaNode    = 0;  // todo get numa
        snprintf(numaBuf, sizeof(numaBuf), "numa%d", numaNode);

        std::string shm_name = "/" + baseSegment + numaBuf + "_" + shaVersion + "_data";

        if (shm_name.length() > 255)
        {
            shm_name = shm_name.substr(0, 255);
        }

        return shm_name;
    }

   public:
    SharedMemoryManager() :
        mmapAddr(nullptr),
        fd(-1),
        mmapSize(0),
        isOwner(false),
        isInit(false) {}

    ~SharedMemoryManager() { cleanup(); }


    static bool check_exists(const std::string& username, const std::string& shaVersion) {
        std::string name = build_shm_name(username, shaVersion);

        int fd = shm_open(name.c_str(), O_RDONLY, 0);
        if (fd == -1)
        {
            return false;
        }

        close(fd);
        return true;
    }

    bool read_existing(const std::string& username, const std::string& shaVersion, size_t count) {
        if (isInit)
        {
            return true;
        }

        name = build_shm_name(username, shaVersion);

        fd = shm_open(name.c_str(), O_RDONLY, 0444);
        if (fd == -1)
        {
            std::cerr << "Failed to open existing shared memory object: " << strerror(errno)
                      << std::endl;
            return false;
        }

        elementCount = count;
        mmapSize     = elementCount * sizeof(T);

        mmapAddr = static_cast<T*>(mmap(nullptr, mmapSize, PROT_READ, MAP_SHARED, fd, 0));
        if (mmapAddr == MAP_FAILED)
        {
            std::cerr << "Failed to map shared memory: " << strerror(errno) << std::endl;
            close(fd);
            fd = -1;
            return false;
        }

        isOwner = false;
        isInit  = true;
        return true;
    }

    bool init(const std::string& username,
              const std::string& shaVersion,
              const T*           sourceArray,
              size_t             count) {

        if (isInit)
        {
            return true;
        }

        elementCount = count;
        mmapSize     = elementCount * sizeof(T);
        name         = build_shm_name(username, shaVersion);

        bool exists = check_exists(username, shaVersion);

        fd = shm_open(name.c_str(), O_RDWR | (exists ? 0 : O_CREAT), 0660);
        if (fd == -1)
        {
            std::cerr << "Failed to open shared memory object: " << strerror(errno) << std::endl;
            return false;
        }

        if (!exists)
        {
            if (ftruncate(fd, mmapSize) == -1)
            {
                std::cerr << "Failed to set size of shared memory object: " << strerror(errno)
                          << std::endl;
                close(fd);
                fd = -1;
                return false;
            }
            isOwner = true;
        }

        mmapAddr =
          static_cast<T*>(mmap(nullptr, mmapSize, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0));
        if (mmapAddr == MAP_FAILED)
        {
            std::cerr << "Failed to map shared memory: " << strerror(errno) << std::endl;
            close(fd);
            fd = -1;
            return false;
        }

        // madvise(mmapAddr, mmapSize, MADV_WILLNEED);
        // madvise(mmapAddr, mmapSize, MADV_HUGEPAGE);
        // mlock(mmapAddr, mmapSize);

        if (isOwner && sourceArray != nullptr)
        {
            std::memcpy(mmapAddr, sourceArray, mmapSize);

            if (msync(mmapAddr, mmapSize, MS_SYNC) == -1)
            {
                std::cerr << "Failed to sync shared memory: " << strerror(errno) << std::endl;
            }
        }

        isInit = true;
        return true;
    }

    T* data() const {
        if (!isInit)
        {
            std::cerr << "Warning: Accessing data() on uninitialized SharedMemoryManager"
                      << std::endl;
            return nullptr;
        }
        return mmapAddr;
    }

    size_t size() const { return elementCount; }

    void close_mmap() {
        if (mmapAddr != nullptr && mmapAddr != MAP_FAILED)
        {
            munmap(mmapAddr, mmapSize);
            mmapAddr = nullptr;
        }

        if (fd != -1)
        {
            close(fd);
            fd = -1;
        }
    }

    void cleanup() {
        if (mmapAddr != nullptr && mmapAddr != MAP_FAILED)
        {
            munmap(mmapAddr, mmapSize);
            mmapAddr = nullptr;
        }

        if (fd != -1)
        {
            close(fd);
            fd = -1;
        }

        if (isOwner && !name.empty())
        {
            std::cout << "unlink" << std::endl;
            shm_unlink(name.c_str());
        }

        isInit = false;
    }
};

}  // namespace Stockfish

#endif  // #ifndef MEMORY_H_INCLUDED
