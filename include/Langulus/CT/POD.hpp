///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Abstract.hpp"
#include "Langulus/Typenav.hpp"


namespace Langulus::CTTI
{
   /// Affects CT::POD<T>                                                     
   ///   @note is_trivially_destructible_v is required to strenghten the      
   ///      is_trivial_v check on GCC/Clang due to compiler bugs; MSVC is fine
   ///   @note std::array will be considered POD if containing POD elements   
   ///      so we make sure that ranges are never considered POD by default,  
   ///      otherwise an array containing one hash will result in a rehash    
   ///      instead of a reuse                                                
   ///   @note extents are ignored by default                                 
   template<class T>
   struct POD {
      static constexpr bool Default = true;
      static constexpr bool Enabled = not CT::Abstract<DeextAll<T>> and (
         CT::Sparse<DeextAll<T>> or CT::Fundamental<DeextAll<T>> or (
                ::std::is_trivial_v<DeextAll<T>>
            and ::std::is_standard_layout_v<DeextAll<T>>
            and ::std::is_trivially_destructible_v<DeextAll<T>>
            and not ::std::ranges::range<DeextAll<T>>
         )
      );
   };
}

LANGULUS_CTTI_CONCEPT_DECVQE(POD);
