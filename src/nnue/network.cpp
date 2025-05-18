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

#include "network.h"

#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <type_traits>
#include <vector>

#define INCBIN_SILENCE_BITCODE_WARNING
#include "../incbin/incbin.h"

#include "../evaluate.h"
#include "../memory.h"
#include "../misc.h"
#include "../position.h"
#include "../types.h"
#include "nnue_architecture.h"
#include "nnue_common.h"
#include "nnue_misc.h"

// Macro to embed the default efficiently updatable neural network (NNUE) file
// data in the engine binary (using incbin.h, by Dale Weiler).
// This macro invocation will declare the following three variables
//     const unsigned char        gEmbeddedNNUEData[];  // a pointer to the embedded data
//     const unsigned char *const gEmbeddedNNUEEnd;     // a marker to the end
//     const unsigned int         gEmbeddedNNUESize;    // the size of the embedded file
// Note that this does not work in Microsoft Visual Studio.
#if !defined(_MSC_VER) && !defined(NNUE_EMBEDDING_OFF)
INCBIN(EmbeddedNNUEBig, EvalFileDefaultNameBig);
INCBIN(EmbeddedNNUESmall, EvalFileDefaultNameSmall);
#else
const unsigned char        gEmbeddedNNUEBigData[1]   = {0x0};
const unsigned char* const gEmbeddedNNUEBigEnd       = &gEmbeddedNNUEBigData[1];
const unsigned int         gEmbeddedNNUEBigSize      = 1;
const unsigned char        gEmbeddedNNUESmallData[1] = {0x0};
const unsigned char* const gEmbeddedNNUESmallEnd     = &gEmbeddedNNUESmallData[1];
const unsigned int         gEmbeddedNNUESmallSize    = 1;
#endif

namespace {

struct EmbeddedNNUE {
    EmbeddedNNUE(const unsigned char* embeddedData,
                 const unsigned char* embeddedEnd,
                 const unsigned int   embeddedSize) :
        data(embeddedData),
        end(embeddedEnd),
        size(embeddedSize) {}
    const unsigned char* data;
    const unsigned char* end;
    const unsigned int   size;
};

using namespace Stockfish::Eval::NNUE;

EmbeddedNNUE get_embedded(EmbeddedNNUEType type) {
    if (type == EmbeddedNNUEType::BIG)
        return EmbeddedNNUE(gEmbeddedNNUEBigData, gEmbeddedNNUEBigEnd, gEmbeddedNNUEBigSize);
    else
        return EmbeddedNNUE(gEmbeddedNNUESmallData, gEmbeddedNNUESmallEnd, gEmbeddedNNUESmallSize);
}


}


namespace Stockfish::Eval::NNUE {


namespace {
SharedMemoryManager<char> manager[2];

}

namespace Detail {

// Read evaluation function parameters
template<typename T>
bool read_parameters(std::istream& stream, T& reference) {

    std::uint32_t header;
    header = read_little_endian<std::uint32_t>(stream);
    if (!stream || header != T::get_hash_value())
        return false;
    return reference.read_parameters(stream);
}

// Write evaluation function parameters
template<typename T>
bool write_parameters(std::ostream& stream, T& reference) {

    // write_little_endian<std::uint32_t>(stream, T::get_hash_value());
    // return reference.write_parameters(stream);
    return true;
}

}  // namespace Detail

template<typename Arch, typename Transformer>
Network<Arch, Transformer>::Network(const Network<Arch, Transformer>& other) :
    evalFile(other.evalFile),
    embeddedType(other.embeddedType) {

    // if (other.featureTransformer)
    featureTransformer = other.featureTransformer;
    // featureTransformer = make_unique_large_page<Transformer>(*other.featureTransformer);

    // network = make_unique_aligned<Arch[]>(LayerStacks);


    // if (!other.network)
    //     return;

    for (std::size_t i = 0; i < LayerStacks; ++i)
        network[i] = other.network[i];
}

template<typename Arch, typename Transformer>
Network<Arch, Transformer>&
Network<Arch, Transformer>::operator=(const Network<Arch, Transformer>& other) {
    evalFile     = other.evalFile;
    embeddedType = other.embeddedType;

    // if (other.featureTransformer)
    featureTransformer = other.featureTransformer;
    // featureTransformer = make_unique_large_page<Transformer>(*other.featureTransformer);

    // network = make_unique_aligned<Arch[]>(LayerStacks);

    // if (!other.network)
    //     return *this;

    for (std::size_t i = 0; i < LayerStacks; ++i)
        network[i] = other.network[i];

    return *this;
}

template<typename Arch, typename Transformer>
void Network<Arch, Transformer>::load(const std::string& rootDirectory, std::string evalfilePath) {
#if defined(DEFAULT_NNUE_DIRECTORY)
    std::vector<std::string> dirs = {"<internal>", "", rootDirectory,
                                     stringify(DEFAULT_NNUE_DIRECTORY)};
#else
    std::vector<std::string> dirs = {"<internal>", "", rootDirectory};
#endif

    if (evalfilePath.empty())
        evalfilePath = evalFile.defaultName;

    for (const auto& directory : dirs)
    {
        if (evalFile.current != evalfilePath)
        {
            if (directory != "<internal>")
            {
                load_user_net(directory, evalfilePath);
            }

            if (directory == "<internal>" && evalfilePath == evalFile.defaultName)
            {
                load_internal();
            }
        }
    }
}


template<typename Arch, typename Transformer>
bool Network<Arch, Transformer>::save(const std::optional<std::string>& filename) const {
    std::string actualFilename;
    std::string msg;

    if (filename.has_value())
        actualFilename = filename.value();
    else
    {
        if (evalFile.current != evalFile.defaultName)
        {
            msg = "Failed to export a net. "
                  "A non-embedded net can only be saved if the filename is specified";

            sync_cout << msg << sync_endl;
            return false;
        }

        actualFilename = evalFile.defaultName;
    }

    std::ofstream stream(actualFilename, std::ios_base::binary);
    bool          saved = save(stream, evalFile.current, evalFile.netDescription);

    msg = saved ? "Network saved successfully to " + actualFilename : "Failed to export a net";

    sync_cout << msg << sync_endl;
    return saved;
}


template<typename Arch, typename Transformer>
NetworkOutput
Network<Arch, Transformer>::evaluate(const Position&                         pos,
                                     AccumulatorStack&                       accumulatorStack,
                                     AccumulatorCaches::Cache<FTDimensions>* cache) const {

    constexpr uint64_t alignment = CacheLineSize;

    alignas(alignment)
      TransformedFeatureType transformedFeatures[FeatureTransformer<FTDimensions>::BufferSize];

    ASSERT_ALIGNED(transformedFeatures, alignment);

    const int  bucket = (pos.count<ALL_PIECES>() - 1) / 4;
    const auto psqt =
      featureTransformer.transform(pos, accumulatorStack, cache, transformedFeatures, bucket);
    const auto positional = network[bucket].propagate(transformedFeatures);
    return {static_cast<Value>(psqt / OutputScale), static_cast<Value>(positional / OutputScale)};
}


template<typename Arch, typename Transformer>
void Network<Arch, Transformer>::verify(std::string                                  evalfilePath,
                                        const std::function<void(std::string_view)>& f) const {
    if (evalfilePath.empty())
        evalfilePath = evalFile.defaultName;

    if (evalFile.current != evalfilePath)
    {
        if (f)
        {
            std::string msg1 =
              "Network evaluation parameters compatible with the engine must be available.";
            std::string msg2 = "The network file " + evalfilePath + " was not loaded successfully.";
            std::string msg3 = "The UCI option EvalFile might need to specify the full path, "
                               "including the directory name, to the network file.";
            std::string msg4 = "The default net can be downloaded from: "
                               "https://tests.stockfishchess.org/api/nn/"
                             + evalFile.defaultName;
            std::string msg5 = "The engine will be terminated now.";

            std::string msg = "ERROR: " + msg1 + '\n' + "ERROR: " + msg2 + '\n' + "ERROR: " + msg3
                            + '\n' + "ERROR: " + msg4 + '\n' + "ERROR: " + msg5 + '\n';

            f(msg);
        }

        exit(EXIT_FAILURE);
    }

    if (f)
    {
        size_t size = sizeof(featureTransformer) + sizeof(Arch) * LayerStacks;
        f("NNUE evaluation using " + evalfilePath + " (" + std::to_string(size / (1024 * 1024))
          + "MiB, (" + std::to_string(featureTransformer.InputDimensions) + ", "
          + std::to_string(network[0].TransformedFeatureDimensions) + ", "
          + std::to_string(network[0].FC_0_OUTPUTS) + ", " + std::to_string(network[0].FC_1_OUTPUTS)
          + ", 1))");
    }
}


template<typename Arch, typename Transformer>
NnueEvalTrace
Network<Arch, Transformer>::trace_evaluate(const Position&                         pos,
                                           AccumulatorStack&                       accumulatorStack,
                                           AccumulatorCaches::Cache<FTDimensions>* cache) const {

    constexpr uint64_t alignment = CacheLineSize;

    alignas(alignment)
      TransformedFeatureType transformedFeatures[FeatureTransformer<FTDimensions>::BufferSize];

    ASSERT_ALIGNED(transformedFeatures, alignment);

    NnueEvalTrace t{};
    t.correctBucket = (pos.count<ALL_PIECES>() - 1) / 4;
    for (IndexType bucket = 0; bucket < LayerStacks; ++bucket)
    {
        const auto materialist =
          featureTransformer.transform(pos, accumulatorStack, cache, transformedFeatures, bucket);
        const auto positional = network[bucket].propagate(transformedFeatures);

        t.psqt[bucket]       = static_cast<Value>(materialist / OutputScale);
        t.positional[bucket] = static_cast<Value>(positional / OutputScale);
    }

    return t;
}


template<typename Arch, typename Transformer>
void Network<Arch, Transformer>::load_user_net(const std::string& dir,
                                               const std::string& evalfilePath) {
    std::ifstream stream(dir + evalfilePath, std::ios::binary);
    auto          description = load(stream);

    if (description.has_value())
    {
        evalFile.current        = evalfilePath;
        evalFile.netDescription = description.value();
    }
}


template<typename Arch, typename Transformer>
void Network<Arch, Transformer>::load_internal() {
    // C++ way to prepare a buffer for a memory stream
    class MemoryBuffer: public std::basic_streambuf<char> {
       public:
        MemoryBuffer(char* p, size_t n) {
            setg(p, p, p + n);
            setp(p, p + n);
        }
    };

    const auto embedded = get_embedded(embeddedType);

    MemoryBuffer buffer(const_cast<char*>(reinterpret_cast<const char*>(embedded.data)),
                        size_t(embedded.size));

    std::istream stream(&buffer);
    auto         description = load(stream);

    if (description.has_value())
    {
        evalFile.current        = evalFile.defaultName;
        evalFile.netDescription = description.value();
    }
}


template<typename Arch, typename Transformer>
void Network<Arch, Transformer>::initialize() {
    // featureTransformer = make_unique_large_page<Transformer>();
    // network            = make_unique_aligned<Arch[]>(LayerStacks);
}


template<typename Arch, typename Transformer>
bool Network<Arch, Transformer>::save(std::ostream&      stream,
                                      const std::string& name,
                                      const std::string& netDescription) const {
    if (name.empty() || name == "None")
        return false;

    return write_parameters(stream, netDescription);
}


template<typename Arch, typename Transformer>
std::optional<std::string> Network<Arch, Transformer>::load(std::istream& stream) {
    initialize();
    std::string netDescription;


    std::uint32_t hashValue;
    if (!read_header(stream, &hashValue, &netDescription))
        return std::nullopt;
    if (hashValue != Network::hash)
        return std::nullopt;

    using nettype = Net<Transformer::OutputDimensions, Arch::FC_0_OUTPUTS, Arch::FC_1_OUTPUTS>;
    constexpr auto netsize = sizeof(nettype);

    std::string username   = getenv("USER") ? getenv("USER") : "default_user";
    std::string shaVersion = std::to_string(hashValue);

    constexpr auto HalfDimensions  = Transformer::OutputDimensions;
    constexpr auto InputDimensions = Transformer::InputDimensions;

    std::cout << "Loading " << username << " " << shaVersion << std::endl;
    bool jumped = false;

start:
    if (manager[int(embeddedType)].check_exists(username, shaVersion))
    {
        std::cout << "Loading from shared memory" << std::endl;
        if (manager[int(embeddedType)].read_existing(username, shaVersion, netsize))
        {
            std::cout << "Shared memory loaded successfully" << std::endl;
            char* data = manager[int(embeddedType)].data();

            size_t offset              = 0;
            featureTransformer.weights = reinterpret_cast<int16_t*>(data + offset);
            offset += sizeof(nettype::FeatureWeights);

            featureTransformer.biases = reinterpret_cast<int16_t*>(data + offset);
            offset += sizeof(nettype::FeatureBiases);

            featureTransformer.psqtWeights = reinterpret_cast<int32_t*>(data + offset);
            offset += sizeof(nettype::PsqtWeights);

            ASSERT_ALIGNED(featureTransformer.weights, 64);
            ASSERT_ALIGNED(featureTransformer.biases, 64);
            ASSERT_ALIGNED(featureTransformer.psqtWeights, 64);

            for (std::size_t i = 0; i < LayerStacks; ++i)
            {
                auto align64 = [&offset, data]() {
                    // Align to 64-byte boundary
                    offset = (offset + 63) & ~63;
                    ASSERT_ALIGNED(data + offset, 64);
                    return data + offset;
                };

                network[i].fc_0.update_w(reinterpret_cast<int8_t*>(align64()));
                offset += sizeof(nettype::Layers[i].L1Weights);

                network[i].fc_0.update_b(reinterpret_cast<int32_t*>(align64()));
                offset += sizeof(nettype::Layers[i].L1Biases);

                network[i].fc_1.update_w(reinterpret_cast<int8_t*>(align64()));
                offset += sizeof(nettype::Layers[i].L2Weights);

                network[i].fc_1.update_b(reinterpret_cast<int32_t*>(align64()));
                offset += sizeof(nettype::Layers[i].L2Biases);

                network[i].fc_2.update_w(reinterpret_cast<int8_t*>(align64()));
                offset += sizeof(nettype::Layers[i].L3Weights);

                network[i].fc_2.update_b(reinterpret_cast<int32_t*>(align64()));
                offset += sizeof(nettype::Layers[i].L3Biases);
            }
        }

        return netDescription;
    }

    if (jumped)
        return std::nullopt;

    std::cout << "Loading from file" << std::endl;

    nnueParams = std::make_unique<nettype>();

    // std::uint32_t header;
    // header =
    read_little_endian<std::uint32_t>(stream);

    read_leb_128<int16_t>(stream, nnueParams->FeatureBiases, HalfDimensions);
    read_leb_128<int16_t>(stream, nnueParams->FeatureWeights, HalfDimensions * InputDimensions);
    read_leb_128<int32_t>(stream, nnueParams->PsqtWeights, PSQTBuckets * InputDimensions);

    Transformer::permute_weights(nnueParams->FeatureBiases, nnueParams->FeatureWeights);
    Transformer::scale_weights(nnueParams->FeatureBiases, nnueParams->FeatureWeights, true);

    for (std::size_t k = 0; k < LayerStacks; ++k)
    {
        auto OutputDimensions      = Arch::L1Type::OutputDimensions;
        auto PaddedInputDimensions = Arch::L1Type::PaddedInputDimensions;

        //header =
        read_little_endian<std::uint32_t>(stream);

        // fc0
        read_little_endian<int32_t>(stream, nnueParams->Layers[k].L1Biases, OutputDimensions);
        for (IndexType i = 0; i < OutputDimensions * PaddedInputDimensions; ++i)
            nnueParams->Layers[k].L1Weights[Arch::L1Type::get_weight_index(i)] =
              read_little_endian<int8_t>(stream);

        OutputDimensions      = Arch::L2Type::OutputDimensions;
        PaddedInputDimensions = Arch::L2Type::PaddedInputDimensions;

        // fc1
        read_little_endian<int32_t>(stream, nnueParams->Layers[k].L2Biases, OutputDimensions);
        for (IndexType i = 0; i < OutputDimensions * PaddedInputDimensions; ++i)
            nnueParams->Layers[k].L2Weights[Arch::L2Type::get_weight_index(i)] =
              read_little_endian<int8_t>(stream);

        OutputDimensions      = Arch::L3Type::OutputDimensions;
        PaddedInputDimensions = Arch::L3Type::PaddedInputDimensions;

        // fc2
        read_little_endian<int32_t>(stream, nnueParams->Layers[k].L3Biases, OutputDimensions);
        for (IndexType i = 0; i < OutputDimensions * PaddedInputDimensions; ++i)
            nnueParams->Layers[k].L3Weights[Arch::L3Type::get_weight_index(i)] =
              read_little_endian<int8_t>(stream);
    }

    featureTransformer.biases      = nnueParams->FeatureBiases;
    featureTransformer.weights     = nnueParams->FeatureWeights;
    featureTransformer.psqtWeights = nnueParams->PsqtWeights;

    for (std::size_t i = 0; i < LayerStacks; ++i)
    {
        network[i].fc_0.update_w(nnueParams->Layers[i].L1Weights);
        network[i].fc_0.update_b(nnueParams->Layers[i].L1Biases);


        network[i].fc_1.update_w(nnueParams->Layers[i].L2Weights);
        network[i].fc_1.update_b(nnueParams->Layers[i].L2Biases);


        network[i].fc_2.update_w(nnueParams->Layers[i].L3Weights);
        network[i].fc_2.update_b(nnueParams->Layers[i].L3Biases);
    }

    // update


    std::cout << "Loading from file done" << std::endl;
    std::cout << "Saving to shared memory" << std::endl;

    std::vector<char> buffer;

    auto append = [&buffer](void* data, size_t size) {
        buffer.insert(buffer.end(), reinterpret_cast<char*>(data),
                      reinterpret_cast<char*>(data) + size);

        // Align to 64-byte boundary
        size_t padding = (64 - (buffer.size() % 64)) % 64;
        if (padding > 0)
            buffer.insert(buffer.end(), padding, 0);
    };

    append(nnueParams->FeatureWeights, sizeof(nnueParams->FeatureWeights));
    append(nnueParams->FeatureBiases, sizeof(nnueParams->FeatureBiases));
    append(nnueParams->PsqtWeights, sizeof(nnueParams->PsqtWeights));

    for (std::size_t i = 0; i < LayerStacks; ++i)
    {
        append(nnueParams->Layers[i].L1Weights, sizeof(nnueParams->Layers[i].L1Weights));
        append(nnueParams->Layers[i].L1Biases, sizeof(nnueParams->Layers[i].L1Biases));
        append(nnueParams->Layers[i].L2Weights, sizeof(nnueParams->Layers[i].L2Weights));
        append(nnueParams->Layers[i].L2Biases, sizeof(nnueParams->Layers[i].L2Biases));
        append(nnueParams->Layers[i].L3Weights, sizeof(nnueParams->Layers[i].L3Weights));
        append(nnueParams->Layers[i].L3Biases, sizeof(nnueParams->Layers[i].L3Biases));
    }
    nnueParams.reset();

    std::cout << "Size of the buffer: " << netsize << " " << buffer.size() << std::endl;

    manager[int(embeddedType)].init(username, shaVersion, buffer.data(), buffer.size());

    jumped = true;
    goto start;

    return netDescription;
}


// Read network header
template<typename Arch, typename Transformer>
bool Network<Arch, Transformer>::read_header(std::istream&  stream,
                                             std::uint32_t* hashValue,
                                             std::string*   desc) const {
    std::uint32_t version, size;

    version    = read_little_endian<std::uint32_t>(stream);
    *hashValue = read_little_endian<std::uint32_t>(stream);
    size       = read_little_endian<std::uint32_t>(stream);
    if (!stream || version != Version)
        return false;
    desc->resize(size);
    stream.read(&(*desc)[0], size);
    return !stream.fail();
}


// Write network header
template<typename Arch, typename Transformer>
bool Network<Arch, Transformer>::write_header(std::ostream&      stream,
                                              std::uint32_t      hashValue,
                                              const std::string& desc) const {
    write_little_endian<std::uint32_t>(stream, Version);
    write_little_endian<std::uint32_t>(stream, hashValue);
    write_little_endian<std::uint32_t>(stream, std::uint32_t(desc.size()));
    stream.write(&desc[0], desc.size());
    return !stream.fail();
}


template<typename Arch, typename Transformer>
bool Network<Arch, Transformer>::read_parameters(std::istream& stream,
                                                 std::string&  netDescription) const {
    // std::uint32_t hashValue;
    // if (!read_header(stream, &hashValue, &netDescription))
    //     return false;
    // if (hashValue != Network::hash)
    //     return false;f
    // if (!Detail::read_parameters(stream, *featureTransformer))
    //     return false;
    // for (std::size_t i = 0; i < LayerStacks; ++i)
    // {
    //     if (!Detail::read_parameters(stream, Layers[i].network))
    //         return false;
    // }
    // return stream && stream.peek() == std::ios::traits_type::eof();
    return true;
}


template<typename Arch, typename Transformer>
bool Network<Arch, Transformer>::write_parameters(std::ostream&      stream,
                                                  const std::string& netDescription) const {
    // if (!write_header(stream, Network::hash, netDescription))
    //     return false;
    // if (!Detail::write_parameters(stream, *featureTransformer))
    //     return false;
    // for (std::size_t i = 0; i < LayerStacks; ++i)
    // {
    //     if (!Detail::write_parameters(stream, Layers[i].network))
    //         return false;
    // }
    // return bool(stream);
    return true;
}

// Explicit template instantiations

template class Network<NetworkArchitecture<TransformedFeatureDimensionsBig, L2Big, L3Big>,
                       FeatureTransformer<TransformedFeatureDimensionsBig>>;

template class Network<NetworkArchitecture<TransformedFeatureDimensionsSmall, L2Small, L3Small>,
                       FeatureTransformer<TransformedFeatureDimensionsSmall>>;

}  // namespace Stockfish::Eval::NNUE
