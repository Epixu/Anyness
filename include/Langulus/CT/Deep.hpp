///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../CTTI.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Deep<T>:                        
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Deep = Yes/No;` in T                       
   template<class T>
   struct Deep {
      static constexpr bool Enabled = false;
   };
   
} // namespace Langulus::CTTI

namespace Langulus::CT
{
   
   /// Deep types are reflected as iteratable, and functions are executed in  
   /// each of their contained items instead on the container itself          
   template<class...T>
   concept Deep = ((CTTI::Deep<Shed<T>>::Enabled or Shed<T>::CTTI_Deep::Enabled) and ...);

   /// Flat types are reflected as non-iteratable                             
   template<class...T>
   concept Flat = ((not Deep<T>) and ...);

} // namespace Langulus::CT
