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
#include "Assume.hpp"
#include "CT/Support.hpp"
#include "CT/POD.hpp"
#include "CT/Same.hpp"
#include <bit>
#include <type_traits>
#include <array>
#include <vector>


namespace Langulus
{
   ///                                                                        
   /// Type that holds a hash.                                                
   /// Size is configurable using LANGULUS_HASHSIZE.                          
   ///   @attention missing hash always has value of 0, and a hashing function
   ///      is very unlikely to ever result in a zero hash, so we can easily  
   ///      detect whether a hash has been generated or not. There's however  
   ///      a very very very small chance, that a hash will end up being      
   ///      constantly regenerated, if it ends up as 0 after hashing.         
   ///   @attention hashing a single hash always returns the hash itself      
   ///                                                                        
   struct Hash {
      using CTTI_POD      = Yes<>;
      using CTTI_Nullable = Yes<>;

      #if LANGULUS_HASHSIZE == 32
         using InnerType = uint32_t;
      #elif LANGULUS_HASHSIZE == 64
         using InnerType = uint64_t;
      #else
         #error "Unsupported hash size"
      #endif

      InnerType value;
      
      explicit constexpr operator bool() const noexcept { return value != 0; }
      constexpr bool operator == (const Hash&) const noexcept = default;
      constexpr bool operator == (InnerType v) const noexcept { return value == v; }
      
      constexpr Hash() noexcept = default;
      constexpr Hash(Hash const&) noexcept = default;
      constexpr Hash(Hash &&) noexcept = default;
      constexpr Hash(InnerType v) noexcept : value(v) {}
      constexpr Hash& operator = (InnerType v) noexcept { value = v; return *this; }
      constexpr Hash& operator = (Hash const&) noexcept = default;
      constexpr Hash& operator = (Hash&&) noexcept = default;
   };

   /// Default hash seed used in Langulus                                     
   constexpr Hash DefaultHashSeed {19890212};

   namespace Inner
   {
      ///                                                                     
      /// MurmurHash3 was written by Austin Appleby, and is placed in the     
      /// public domain                                                       
      ///                                                                     
      /// Note1 - The x86 and x64 versions do _not_ produce the same results, 
      /// as the algorithms are optimized for their respective platforms.     
      /// You can still compile and run any of them on any platform, but your 
      /// performance with the non-native version will be less than optimal.  
      ///                                                                     
      /// Note2 - These are constexpr-friendly versions, made by Tamás Szelei 
      /// and slightly modified by me:                                        
      /// https://github.com/sztomi/constexpr_murmurhash                      
      ///                                                                     
      /// A data view that allows us to modify in blocks of 32 bits           
      struct data_view {
      private:
         const uint8_t* p;
         std::size_t sz;

      public:
         constexpr data_view(const uint8_t* a, size_t N) noexcept
            : p(a), sz(N) {}

         template<size_t N>
         constexpr data_view(const ::std::array<uint8_t, N>& a) noexcept
            : p(a.data()), sz(N) {}

         constexpr char operator[](std::size_t n) const has_assumptions {
            LglsAssumeDevAndOptimize(n < sz, "Out of range");
            return p[n];
         }

         constexpr uint32_t get_block(std::size_t idx) const noexcept {
            std::size_t i = (block_size() + idx) * 4u;
            auto b0 = static_cast<uint32_t>(p[i]);
            auto b1 = static_cast<uint32_t>(p[i + 1u]);
            auto b2 = static_cast<uint32_t>(p[i + 2u]);
            auto b3 = static_cast<uint32_t>(p[i + 3u]);
            return (b3 << 24u) | (b2 << 16u) | (b1 << 8u) | b0;
         }

         constexpr std::size_t size() const noexcept { return sz; }

         constexpr std::size_t block_size() const noexcept { return sz / 4u; }

         constexpr uint8_t tail(const std::size_t n) const noexcept {
            std::size_t tail_size = sz % 4u;
            return p[sz - tail_size + n];
         }
      };

      ///                                                                     
      /// 32-bit hasher                                                       
      ///                                                                     
      constexpr uint32_t mm3_x86_32(data_view key, uint32_t seed) {
         uint32_t h1 = seed;

         constexpr uint32_t c1 = 0xcc9e2d51;
         constexpr uint32_t c2 = 0x1b873593;

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

         // ReSharper disable once CppDefaultCaseNotHandledInSwitchStatement
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
         }

         h1 ^= key.size();

         h1 ^= h1 >> 16;
         h1 *= 0x85ebca6b;
         h1 ^= h1 >> 13;
         h1 *= 0xc2b2ae35;
         h1 ^= h1 >> 16;

         return h1;
      }
   }


   /// Hash a sequence of bytes                                               
   ///   @param data - the data to hash                                       
   ///   @param seed - the seed                                               
   ///   @return the hash                                                     
   constexpr Hash HashBytes(Inner::data_view data, Hash seed) noexcept {
      static_assert(sizeof(Hash) == 4, "Not implemented");
      return Hash {Inner::mm3_x86_32(data, seed.value)};
   }

   /// Predeclaration required by CT::Hashable                                
   template<bool FAKE = false, Hash SEED = DefaultHashSeed, class T, class...MORE>
   constexpr auto HashOf(T&&, MORE&&...);

   namespace CT
   {
      /// Check if the origin T can be hashed using HashOf                    
      template<class...T>
      concept Hashable = Validate<T...> and requires (Shed<T>...a) {
         { (HashOf<true>(a), ...) } -> Supported;
      };

      template<class...T>
      concept NotHashable = Validate<T...> and ((not Hashable<Shed<T>>) and ...);

      /// Check if T has a GetHash() method                                   
      /// It is always preferred when hashing data                            
      template<class...T>
      concept HasGetHashMethod = Validate<T...> and requires (Shed<T>...a) {
         { (a.GetHash(), ...) } -> Similar<Hash>;
      };
      
      /// Check if T has a GetHash() method                                   
      /// It is always preferred when hashing data                            
      template<class...T>
      concept HasStdHasher = Validate<T...>
          and requires (::std::hash<Shed<T>>...h, Shed<T>...a) { (h(a), ...); };
   }


   /// Hash any hashable data, including fundamental/POD/range types          
   ///   @tparam FORCE_RUNTIME - for internal use - if FORCE_RUNTIME and      
   ///      evaluated to fail, it will return CT::Unsupported; otherwise it   
   ///      will scream a compiler error at you. Being true also forces the   
   ///      hash to be performed at runtime, so that it doesn't fail on       
   ///      CT::Hashable checks at reflection time                            
   ///   @tparam SEED - the seed for the hash algorithm                       
   ///   @param head, rest - the data to hash                                 
   ///   @return the hash                                                     
   template<bool FORCE_RUNTIME, Hash SEED, class T, class...MORE>
   constexpr auto HashOf(T&& head, MORE&&...rest) {
      static_assert(not CT::Sheddable<T, MORE...>,
         "Shed all sheddable wrappers before hashing");

      if constexpr (CT::Unsupported<T, MORE...>) {
         // If any of the types isn't supported abort the entire hash   
         return Unsupported {};
      }
      else if constexpr (sizeof...(MORE)) {
         // Combine all data into a single array of hashes, and then    
         // hash that array as a whole                                  
         const Hash coal[1 + sizeof...(MORE)] {
            HashOf<FORCE_RUNTIME, SEED>(head),
            HashOf<FORCE_RUNTIME, SEED>(rest)...
         };

         if consteval {
            if constexpr (FORCE_RUNTIME)
               return HashBytes({reinterpret_cast<const uint8_t*>(coal), sizeof(coal)}, SEED);
            else {
               auto as_bytes = ::std::bit_cast<::std::array<uint8_t, sizeof(coal)>>(coal);
               return HashBytes(as_bytes, SEED);
            }
         }
         else {
            return HashBytes({reinterpret_cast<const uint8_t*>(coal), sizeof(coal)}, SEED);
         }
      }
      else if constexpr (CT::Array<T>) {
         using InnerT = Deext<T>;

         // Combine the hashes of each element inside an array          
         if constexpr (ExtentOf<T> == 1) {
            // Only one element in array, just use the first element    
            return HashOf<FORCE_RUNTIME, SEED>(head[0]);
         }
         else if constexpr (CT::POD<InnerT> and not CT::HasGetHashMethod<InnerT>) {
            // Array is made of POD elements, batch-hash the array      
            return HashBytes({head, sizeof(head)}, SEED);
         }
         else {
            // Hash each element of the array individually, and then    
            // hash that array of hashes as a whole                     
            Hash coal[ExtentOf<T>];
            for (size_t i = 0; i < ExtentOf<T>; ++i)
               coal[i] = HashOf<FORCE_RUNTIME, SEED>(head[i]);

            if consteval {
               if constexpr (FORCE_RUNTIME)
                  return HashBytes({reinterpret_cast<const uint8_t*>(coal), sizeof(coal)}, SEED);
               else {
                  auto as_bytes = ::std::bit_cast<::std::array<uint8_t, sizeof(coal)>>(coal);
                  return HashBytes(as_bytes, SEED);
               }
            }
            else {
               return HashBytes({reinterpret_cast<const uint8_t*>(coal), sizeof(coal)}, SEED);
            }
         }
      }
      else if constexpr (::std::is_pointer_v<T>) {
         // Hash pointer, never dereference it                          
         if consteval {
            if constexpr (FORCE_RUNTIME)
               return HashBytes({reinterpret_cast<const uint8_t*>(&head), sizeof(T)}, SEED);
            else {
               auto as_bytes = ::std::bit_cast<::std::array<uint8_t, sizeof(T)>>(head);
               return HashBytes(as_bytes, SEED);
            }
         }
         else {
            return HashBytes({reinterpret_cast<const uint8_t*>(&head), sizeof(T)}, SEED);
         }
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
         if consteval {
            if constexpr (FORCE_RUNTIME)
               return HashBytes({reinterpret_cast<const uint8_t*>(&head), sizeof(T)}, SEED);
            else {
               auto as_bytes = ::std::bit_cast<::std::array<uint8_t, sizeof(T)>>(head);
               return HashBytes(as_bytes, SEED);
            }
         }
         else {
            return HashBytes({reinterpret_cast<const uint8_t*>(&head), sizeof(T)}, SEED);
         }
      }
      else if constexpr (::std::ranges::range<T> and CT::Hashable<TypeOf<T>>) {
         // Anything that is range-iteratable and typed is carried      
         // through HashOf for consistency, because different std       
         // library implementations might have different hashing        
         // algorithms. This should include string_view, string, vector,
         // array, span, etc.                                           
         using InnerT = TypeOf<T>;

         // If it is just a single element, we can directly hash it     
         if (head.size() == 1)
            return HashOf(*head.begin());

         if constexpr (::std::ranges::contiguous_range<T> and CT::POD<InnerT> and not CT::HasGetHashMethod<InnerT>) {
            // Batch-hash contiguous containers with POD contents       
            if constexpr (requires { ::std::bit_cast<::std::array<char, sizeof(T)>>(head); }) {
               // Constant evaluation is possible only if bit_cast      
               // is able to do its magic                               
               if consteval {
                  if constexpr (FORCE_RUNTIME)
                     return HashBytes({reinterpret_cast<const uint8_t*>(head.data()), head.size() * sizeof(InnerT)}, SEED);
                  else {
                     auto as_bytes = ::std::bit_cast<::std::array<uint8_t, sizeof(T)>>(head);
                     return HashBytes(as_bytes, SEED);
                  }
               }
               else {
                  return HashBytes({reinterpret_cast<const uint8_t*>(head.data()), head.size() * sizeof(InnerT)}, SEED);
               }
            }
            else {
               return HashBytes({reinterpret_cast<const uint8_t*>(head.data()), head.size() * sizeof(InnerT)}, SEED);
            }
         }
         else {
            // Hash each individual element, then combine all hashes    
            // Possible only at runtime                                 
            ::std::vector<Hash> coal;
            for (auto& i : head)
               coal.emplace_back(HashOf<FORCE_RUNTIME, SEED>(i));

            return HashBytes({reinterpret_cast<const uint8_t*>(coal.data()), coal.size() * sizeof(Hash)}, SEED);
         }
      }      
      else if constexpr (CT::HasStdHasher<T>) {
         // Hashable via std::hash (fallback for std containers)        
         // Beware, hashing functions coming from std::hash may have    
         // different implementations for different compilers, which    
         // will likely result in different ordering inside unsorted    
         // containers. Nothing serious, unless you're pedantic like me 
         ::std::hash<T> hasher;
         return Hash {hasher(head)};
      }
      else {
         // Handle failure statically                                   
         static_assert(FORCE_RUNTIME, "Can't hash data");
         return Unsupported {};
      }
   }
}


namespace std
{
   /// Extend std to be capable of hashing anything with a GetHash method     
   template<::Langulus::CT::HasGetHashMethod H>
   struct hash<H> {
      LANGULUS(INLINED)
      size_t operator()(const H& what) const noexcept {
         return what.GetHash().value;
      }
   };
}
