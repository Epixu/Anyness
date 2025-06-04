///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Core.hpp"
#include "Typenav.hpp"
#include "TypeOf.hpp"
#include "CT/Support.hpp"
#include "CT/POD.hpp"
#include "CT/Same.hpp"


namespace Langulus
{
   
   ///                                                                        
   /// Type that holds a hash                                                 
   /// Size is configurable using LANGULUS_HASH64 or LANGULUS_HASH32 defines  
   ///                                                                        
   ///   @attention missing hash always has value of 0, and a hashing function
   ///      is very unlikely to ever result in a zero hash, so we can easily  
   ///      detect whether a hash has been generated or not. There's however  
   ///      a very very very small chance, that a hash will end up being      
   ///      constantly regenerated, if it ends up as 0 after hashing          
   ///   @attention hashing a single hash always returns the hash itself      
   ///                                                                        
   struct Hash {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      #if not defined(LANGULUS_HASH64)
         uint32_t mHash = 0;
      #elif defined(LANGULUS_HASH64) and not defined(LANGULUS_HASH32)
         uint64_t mHash = 0;
      #else
         #error Conflicting hash type definitions
      #endif

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator bool() const noexcept {
         return mHash != 0;
      }

      constexpr bool operator == (const Hash&) const noexcept = default;
   };

   /// Default hash seed used in Langulus                                     
   constexpr Hash DefaultHashSeed {19890212};

   namespace Inner
   {

      /// MurmurHash3 was written by Austin Appleby, and is placed in the     
      /// public domain                                                       
      ///                                                                     
      /// Note1 - The x86 and x64 versions do _not_ produce the same results, 
      /// as the algorithms are optimized for their respective platforms.     
      /// You can still compile and run any of them on any platform, but your 
      /// performance with the non-native version will be less than optimal.  
      ///                                                                     
      /// Note2 - These are constexpr-friendly versions, made by Tamás Szelei 
      /// and slightly modified by me                                         
      /// https://github.com/sztomi/constexpr_murmurhash                      
      class str_view {
      public:
         template <std::size_t N>
         constexpr str_view(const char(&a)[N])
            : p(a), sz(N - 1) {}

         constexpr char operator[](std::size_t n) const {
            return n < sz ? p[n] : throw std::out_of_range("");
         }

         constexpr uint32_t get_block(int idx) {
            int i = (block_size() + idx) * 4;
            uint32_t b0 = p[i];
            uint32_t b1 = p[i + 1];
            uint32_t b2 = p[i + 2];
            uint32_t b3 = p[i + 3];
            return (b3 << 24) | (b2 << 16) | (b1 << 8) | b0;
         }

         constexpr std::size_t size() const { return sz; }

         constexpr std::size_t block_size() const { return sz / 4; }

         constexpr char tail(const int n) const {
            int tail_size = sz % 4;
            return p[sz - tail_size + n];
         }

      private:
         const char* p;
         std::size_t sz;
      };

      constexpr uint32_t mm3_x86_32(str_view key, uint32_t seed) {
         uint32_t h1 = seed;

         const uint32_t c1 = 0xcc9e2d51;
         const uint32_t c2 = 0x1b873593;

         const int nblocks = key.size() / 4;
         for (int i = -nblocks; i; i++) {
            uint32_t k1 = key.get_block(i);

            k1 *= c1;
            k1 = (k1 << 15) | (k1 >> (32 - 15));
            k1 *= c2;

            h1 ^= k1;
            h1 = (h1 << 13) | (h1 >> (32 - 13));
            h1 = h1 * 5 + 0xe6546b64;
         }

         uint32_t k1 = 0;

         switch (key.size() & 3) {
         case 3:
            k1 ^= key.tail(2) << 16;
         case 2:
            k1 ^= key.tail(1) << 8;
         case 1:
            k1 ^= key.tail(0);
            k1 *= c1;
            k1 = (k1 << 15) | (k1 >> (32 - 15));
            k1 *= c2;
            h1 ^= k1;
         };

         h1 ^= key.size();

         h1 ^= h1 >> 16;
         h1 *= 0x85ebca6b;
         h1 ^= h1 >> 13;
         h1 *= 0xc2b2ae35;
         h1 ^= h1 >> 16;

         return h1;
      }

   } // namespace Langulus::Inner


   /// Hash a sequence of bytes                                               
   ///   @tparam SEED - the seed for the hash algorithm                       
   ///   @tparam TAIL - true for a generalized hashing routine (internal)     
   ///   @param ptr - memory start                                            
   ///   @param len - number of bytes to hash                                 
   ///   @return the hash                                                     
   template<Hash SEED = DefaultHashSeed, bool TAIL = true>
   constexpr Hash HashBytes(void const* ptr, int len) noexcept {
      Hash result;
      if constexpr (sizeof(Hash) == 4)
         Inner::MurmurHash3_x86_32<TAIL, SEED>(ptr, len, &result);
      else if constexpr (sizeof(Hash) == 8)
         Inner::MurmurHash2_x64_64<TAIL, SEED>(ptr, len, &result);
      else if constexpr (sizeof(Hash) == 16)
         Inner::MurmurHash3_x64_128<TAIL, SEED>(ptr, len, &result);
      else
         static_assert(false, "Not implemented");
      return result;
   }

   namespace CT
   {

      /// Check if the origin T can be hashed using HashOf                    
      template<class...T>
      concept Hashable = requires (T&...a) {
         { (HashOf<true>(a), ...) } -> Supported;
      };

      /// Check if T has a GetHash() method                                   
      /// It is always preferred when hashing data                            
      template<class...T>
      concept HasGetHashMethod = requires (T&...a) {
         { (a.GetHash(), ...) } -> Similar<Hash>;
      };
      
      /// Check if T has a GetHash() method                                   
      /// It is always preferred when hashing data                            
      template<class...T>
      concept HasStdHasher = requires (::std::hash<T>...h, T...a) {
         (h(a), ...);
      };

   } // namespace Langulus::CT


   /// Hash any hashable data, including fundamental/POD/range types          
   ///   @tparam FAKE - for internal use - if FAKE and evaluated to fail, it  
   ///      will return CT::Unsupported; otherwise it will scream a compile   
   ///      error at you                                                      
   ///   @tparam SEED - the seed for the hash algorithm                       
   ///   @param head, rest... - the data to hash                              
   ///   @return the hash                                                     
   template<bool FAKE = false, Hash SEED = DefaultHashSeed, class T, class...MORE>
   constexpr auto HashOf(const T& head, const MORE&...rest) {
      if constexpr (CT::Unsupported<T, MORE...>) {
         // If any of the types isn't supported abort the entire hash   
         return Unsupported {};
      }
      else if constexpr (sizeof...(MORE)) {
         // Combine all data into a single array of hashes, and then    
         // hash that array as a whole                                  
         alignas(Byteness) const Hash coal[1 + sizeof...(MORE)] {
            HashOf<FAKE, SEED>(head),
            HashOf<FAKE, SEED>(rest)...
         };
         return HashBytes<SEED, false>(coal, static_cast<int>(sizeof(coal)));
      }
      else if constexpr (CT::Array<T>) {
         // Combine the hashes of each element inside an array          
         if constexpr (ExtentOf<T> == 1) {
            // Only one element in array, just use the first element    
            return HashOf<FAKE, SEED>(head[0]);
         }
         else if constexpr (CT::POD<Deext<T>>) {
            // Array is made of POD elements, batch-hash the array      
            return HashBytes<SEED>(head, static_cast<int>(sizeof(T)));
         }
         else {
            // Hash each element of the array individually, and then    
            // hash that array of hashes as a whole                     
            alignas(Byteness) Hash coal[ExtentOf<T>];
            for (::std::size_t i = 0; i < ExtentOf<T>; ++i)
               coal[i] = HashOf<FAKE, SEED>(head[i]);
            return HashBytes<SEED, false>(coal, static_cast<int>(sizeof(coal)));
         }
      }
      else if constexpr (CT::Sparse<T>) {
         // Hash pointer, never dereference it                          
         if (head == nullptr)
            return Hash {};
         return HashBytes<SEED, false>(&head, static_cast<int>(sizeof(T)));
      }
      else if constexpr (CT::Similar<T, Hash>) {
         // Provided type is already a hash, just propagate it          
         return head;
      }
      else if constexpr (CT::HasGetHashMethod<T>) {
         // Hashable via a member GetHash() function                    
         // Allows for caching the hash                                 
         return head.GetHash();
      }
      else if constexpr (CT::POD<T>) {
         // Explicitly marked POD item is always hashable, but be       
         // careful for POD types with padding - the junk inbetween     
         // members can interfere with the hash, giving unique          
         // hashes where the same hashes should be produced. In such    
         // cases it is recommended you add a custom GetHash() method   
         // to your type, or #pragma pack, in order to circumvent issue 
         return HashBytes<SEED, (alignof(T) < Byteness)>(&head, static_cast<int>(sizeof(T)));
      }
      else if constexpr (::std::ranges::range<T> and CT::Hashable<TypeOf<T>>) {
         // Anything that is range-iteratable and typed is carried      
         // through HashOf for consistency, because different std       
         // library implementations might have different hashing        
         // algorithms. This should include string_view, string, vector,
         // array, span, etc.                                           
         using InnerT = TypeOf<T>;

         if constexpr (::std::ranges::contiguous_range<T> and CT::POD<InnerT>) {
            // Batch-hash contiguous containers with POD contents       
            return HashBytes<SEED>(head.data(), static_cast<int>(head.size() * sizeof(InnerT)));
         }
         else {
            // Hash each individual element, then combine all hashes    
            ::std::vector<Hash> coal;
            for (auto& i : head)
               coal.emplace_back(HashOf<FAKE, SEED>(i));
            return HashBytes<SEED>(coal.data(), static_cast<int>(coal.size() * sizeof(Hash)));
         }
      }      
      else if constexpr (CT::HasStdHasher<T>) {
         // Hashable via std::hash (fallback for std containers)        
         // Beware, hashing functions coming from std::hash may have    
         // different implementations for different compilers, which    
         // will likely result in different ordering inside unordered   
         // containers. Nothing serious, unless you're pedantic like me 
         ::std::hash<T> hasher;
         return Hash {hasher(head)};
      }
      else {
         // Handle failure statically                                   
         static_assert(FAKE, "Can't hash data");
         return Unsupported {};
      }
   }

} // namespace Langulus

namespace std
{

   /// Extend std to be capable of hashing anything with a GetHash method     
   template<::Langulus::CT::HasGetHashMethod H>
   struct hash<H> {
      LANGULUS(INLINED)
      size_t operator()(const H& what) const noexcept {
         return what.GetHash().mHash;
      }
   };

} // namespace std
