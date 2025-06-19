#pragma once
#include "../Typenav.hpp"
#include "Signed.hpp"


namespace Langulus
{
   
   ///                                                                        
   /// Different pool tactics you can assign to your data types               
   /// Used primarily for advanced tweaking of a final product                
   /// Pooling works only if LANGULUS_FEATURE(MANAGED_MEMORY) is enabled      
   ///                                                                        
   enum class PoolTactic {
      // Data instances will be pooled in the main pool chain,          
      // unless data was reflected from a boundary that is not MAIN     
      Main = 0,

      // Data instances will be pooled based on their size              
      // There will be pools dedicated for each allocation page size    
      // This effectively narrows the search for entries a bit          
      Size,

      // Data instances will be pooled based on their type              
      // Each data definition will have its own pool chain              
      // This is the default pooling tactic for any data type that      
      // is not reflected inside the RTTI::MainBoundary boundary.       
      // See LANGULUS_RTTI_BOUNDARY for more information on that.       
      Type,

      // While debugging, make sure everything defaults to a type-based 
      // pooling, so that we have more meaningul debug information      
      #if LANGULUS(DEBUG)
         Default = Type   
      #else
         Default = Main
      #endif
   };

   /// Useful for setting CTTI_Pooled                                         
   /// Instructs Fractalloc to pool in the common size-indexed pools          
   ///   @tparam MIN_ALLOC - what's the minimal allocation size in bytes      
   ///   @attention MIN_ALLOC will never be lower than Alignment or the size  
   ///      of the type after reflection. It is always a power-of-two         
   template<unsigned MIN_ALLOC = Alignment>
   struct PooledBySize {
      static constexpr PoolTactic Tactic = PoolTactic::Size;
      static constexpr size_t MinAlloc = MIN_ALLOC;
      static constexpr bool   Enabled  = true;
   };

   /// Useful for setting CTTI_Pooled                                         
   /// Instructs Fractalloc to pool to dedicated type-indexed pools           
   ///   @tparam MIN_ALLOC - what's the minimal allocation size in bytes      
   ///   @tparam MIN_POOL - what's the minimal pool size in bytes             
   ///   @attention MIN_ALLOC will never be lower than Alignment or the size  
   ///      of the type after reflection. It is always a power-of-two         
   ///   @attention MIN_POOL will never be lower than MIN_ALLOC * 256 after   
   ///      reflection. It is always a power-of-two                           
   template<unsigned MIN_ALLOC = Alignment, unsigned MIN_POOL = 1024*1024>
   struct PooledByType {
      static constexpr PoolTactic Tactic = PoolTactic::Type;
      static constexpr size_t MinAlloc = MIN_ALLOC;
      static constexpr size_t MinPool  = MIN_POOL;
      static constexpr bool   Enabled  = true;
   };

   /// Round to the upper power-of-two                                        
   ///   @tparam SAFE - set to true if you want it to throw on overflow       
   ///   @param x - the unsigned integer to round up                          
   ///   @return the closest upper power-of-two to x                          
   template<bool SAFE = false, CT::Unsigned T> LANGULUS(ALWAYS_INLINED)
   constexpr T Roof2(const T x) noexcept(not SAFE) {
      if constexpr (SAFE) {
         constexpr T lastPowerOfTwo = (T {1}) << (T {sizeof(T) * 8 - 1});
         if (x > lastPowerOfTwo)
            throw Exception("Roof2 overflowed", HERE());
      }

      if consteval {
         T n = x;
         --n;
         n |= n >> 1;
         n |= n >> 2;
         n |= n >> 4;
         if constexpr (sizeof(T) > 1)
            n |= n >> 8;
         if constexpr (sizeof(T) > 2)
            n |= n >> 16;
         if constexpr (sizeof(T) > 4)
            n |= n >> 32;
         static_assert(sizeof(T) <= 8, "Not implemented");

         ++n;
         return n;
      }
      else {
         return x <= 1 ? x : static_cast<T>((T {1}) << 
            static_cast<T>(sizeof(T) * 8 - ::std::countl_zero(static_cast<T>(x - 1))));
      }
   }
   
} // namespace Langulus

namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Pooled<T>:                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Pooled = PooledBySize/PooledByType;` in T  
   template<class T>
   struct Pooled {
      static constexpr PoolTactic Tactic = PoolTactic::Default;
      static constexpr size_t MinAlloc = Alignment;
      static constexpr size_t MinPool  = 1024 * 1024;
      static constexpr bool   Enabled  = false;
   };

   /// Specialize for all fundamental types                                   
   template<CT::Fundamental T>
   struct Pooled<T> {
      static constexpr PoolTactic Tactic = PoolTactic::Size;
      static constexpr size_t MinAlloc = Alignment;
      static constexpr size_t MinPool  = 1024 * 1024;
      static constexpr bool   Enabled  = true;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Pooled);

namespace Langulus::CT
{

   ///                                                                        
   template<class T>
   consteval size_t GetMinAlloc() {
      using ST = Shed<T>;
      if constexpr (requires { CTTI::Pooled<ST>::Enabled; }) {
         constexpr size_t minalloc = CTTI::Pooled<ST>::MinAlloc;
         return minalloc < Alignment ? Alignment : minalloc;
      }
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, Pooled)) {
         constexpr size_t minalloc = Decay<ST>::CTTI_Pooled::MinAlloc;
         return minalloc < Alignment ? Alignment : minalloc;
      }
      else return sizeof(T) < Alignment ? Alignment : sizeof(T);
   }
   
   ///                                                                        
   template<class T>
   consteval size_t GetMinPool() {
      using ST = Shed<T>;
      if constexpr (requires { CTTI::Pooled<ST>::Enabled; }) {
         constexpr size_t minpool = Roof2(CTTI::Pooled<ST>::MinPool);
         constexpr size_t minallo = Roof2(GetMinAlloc<ST>());
         return minpool < minallo ? minallo : minpool;
      }
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, Pooled)) {
         constexpr size_t minpool = Roof2(Decay<ST>::CTTI_Pooled::MinPool);
         constexpr size_t minallo = Roof2(GetMinAlloc<ST>());
         return minpool < minallo ? minallo : minpool;
      }
      else return Roof2(GetMinAlloc<ST>() * 256);
   }
   
   ///                                                                        
   template<class T>
   consteval PoolTactic GetPoolTactic() {
      using ST = Shed<T>;
      if constexpr (requires { CTTI::Pooled<ST>::Enabled; })
         return CTTI::Pooled<ST>::Tactic;
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, Pooled))
         return Decay<ST>::CTTI_Pooled::Tactic;
      else
         return PoolTactic::Default;
   }

} // namespace Langulus::CT
