///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"
#include "../Utils/Roof.hpp"

#if not LANGULUS_FEATURE(MANAGED_MEMORY)
   #error "This file shouldn't be included if MANAGED_MEMORY is disabled"
#endif


namespace Langulus
{
   ///                                                                        
   /// Different pool tactics you can assign to your data types.              
   /// Used primarily for advanced tweaking of a final product.               
   /// Pooling works only if LANGULUS_FEATURE(MANAGED_MEMORY) is enabled.     
   ///                                                                        
   enum class PoolTactic {
      // Data instances will be pooled in the main pool chain,          
      // unless data was reflected from a boundary that is not MAIN     
      Main = 0,

      // Data instances will be pooled based on their size.             
      // Works only for types that are smaller than Langulus::Alignment 
      // and have compatible alignment.                                 
      Size,

      // Data instances will be pooled based on their type.             
      // Each data definition will have its own pool chain.             
      // This is the default pooling tactic for any data type that      
      // is reflected from a shared library.                            
      // See Langulus::Boundary for more information on that.           
      Type,

      // While debugging, make sure everything defaults to a type-based 
      // pooling, so that we have more meaningful debug information     
      #if LANGULUS(DEBUG)
         Default = Type   
      #else
         Default = Main
      #endif
   };

   /// Useful for setting CTTI_Pooled                                         
   /// Instructs Fractalloc to pool in the common size-indexed pools          
   ///   @attention MinPool doesn't really have any effect on the type, as    
   ///      it will be pooled by size, in a common pool that is sized to the  
   ///      MinimalPoolSize                                                   
   struct PooledBySize {
      static constexpr auto   Tactic   = PoolTactic::Size;
      static constexpr size_t MinPool  = MinimalPoolSize;
      static constexpr bool   Enabled  = true;
   };

   /// Useful for setting CTTI_Pooled                                         
   /// Instructs Fractalloc to pool to dedicated type-indexed pools           
   ///   @tparam MIN_POOL what's the minimal pool size in bytes               
   template<unsigned MIN_POOL>
   struct PooledByType {
      static_assert(::std::has_single_bit(MIN_POOL),
         "MIN_POOL must be a power-of-two");
      static constexpr auto   Tactic   = PoolTactic::Type;
      static constexpr size_t MinPool  = MIN_POOL;
      static constexpr bool   Enabled  = true;
   };
}

namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Pooled<T>:                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Pooled = PooledBySize/PooledByType;` in T  
   template<class T>
   struct Pooled;

   /// All fundamental types are pooled by size by default                    
   template<CT::Fundamental T>
   struct Pooled<T> {
      static constexpr PoolTactic Tactic = PoolTactic::Size;
      static constexpr size_t MinPool  = MinimalPoolSize;
   };
}

namespace Langulus::CT
{
   /// Get the minimal pool size in bytes at compile time for T               
   template<class T>
   consteval size_t GetMinPool() {
      static_assert(::std::has_single_bit(MinimalPoolSize),
         "MinimalPoolSize must be a power-of-two");

      using ST = Shed<T>;
      if constexpr (Complete<CTTI::Pooled<ST>>) {
         constexpr size_t minpool = Roof2(CTTI::Pooled<ST>::MinPool);
         return minpool < MinimalPoolSize ? MinimalPoolSize : minpool;
      }
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, Pooled)) {
         constexpr size_t minpool = Roof2(Decay<ST>::CTTI_Pooled::MinPool);
         return minpool < MinimalPoolSize ? MinimalPoolSize : minpool;
      }
      else return Roof2(sizeof(ST) * 256 <= MinimalPoolSize
         ? MinimalPoolSize : sizeof(ST) * 256
      );
   }
   
   /// Get the reflected pool tactic for T at compile time                    
   template<class T>
   consteval PoolTactic GetPoolTactic() {
      using ST = Shed<T>;
      PoolTactic result = PoolTactic::Default;
      if constexpr (Complete<CTTI::Pooled<ST>>)
         result = CTTI::Pooled<ST>::Tactic;
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, Pooled))
         result = Decay<ST>::CTTI_Pooled::Tactic;

      if (result == PoolTactic::Main
      and (alignof(ST) > Alignment or sizeof(ST) > Alignment)) {
         // The main pool doesn't accept types with size and alignment  
         // that aren't within Langulus::Alignment limits.              
         result = PoolTactic::Type;
      }
      else if (result == PoolTactic::Size
      and alignof(T) > Alignment) {
         // The size pools don't accept types with alignment            
         // that isn't within Langulus::Alignment limits.               
         result = PoolTactic::Type;
      }
      
      return result;
   }
}
