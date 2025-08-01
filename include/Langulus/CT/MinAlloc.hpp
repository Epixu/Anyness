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


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::MinAlloc<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_MinAlloc = Yes<value>;` in T               
   template<class T>
   struct MinAlloc;
}

namespace Langulus::CT
{
   /// Get the minimal allocation in bytes at compile time for T              
   template<class T>
   consteval size_t GetMinAlloc() {
      static_assert(Roof2(MinimalAllocation),
         "MinimalAllocation must be a power-of-two");
      
      using ST = Shed<T>;
      if constexpr (Complete<CTTI::MinAlloc<ST>>) {
         constexpr size_t minalloc = CTTI::MinAlloc<ST>::Value;
         static_assert(Roof2(minalloc),
            "Reflected MinAlloc must be a power-of-two");
         return minalloc < MinimalAllocation ? MinimalAllocation : minalloc;
      }
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, MinAlloc)) {
         constexpr size_t minalloc = Decay<ST>::CTTI_MinAlloc::Constant;
         static_assert(Roof2(minalloc),
            "Reflected MinAlloc must be a power-of-two");
         return minalloc < MinimalAllocation ? MinimalAllocation : minalloc;
      }
      else return sizeof(T) < MinimalAllocation ? MinimalAllocation : sizeof(T);
   }
}
