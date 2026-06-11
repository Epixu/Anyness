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
      static_assert(::std::has_single_bit(MinimalAllocation),
         "MinimalAllocation must be a power-of-two");
      
      using ST = Shed<T>;
      if constexpr (Complete<CTTI::MinAlloc<ST>>) {
         constexpr size_t minalloc
            = Roof2(static_cast<size_t>(CTTI::MinAlloc<ST>::Value));
         return minalloc < MinimalAllocation ? MinimalAllocation : minalloc;
      }
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, MinAlloc, false)) {
         constexpr size_t minalloc
            = Roof2(static_cast<size_t>(Decay<ST>::CTTI_MinAlloc::Constant));
         return minalloc < MinimalAllocation ? MinimalAllocation : minalloc;
      }
      else return Roof2(sizeof(T) < MinimalAllocation ? MinimalAllocation : sizeof(T));
   }
}
