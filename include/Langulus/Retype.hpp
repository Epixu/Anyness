///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Typenav.hpp"
#include "CT/Template.hpp"


namespace Langulus
{
   namespace Inner
   {
      template<class T, class...AN>
      consteval auto RetypeInner() {
         if constexpr (requires { typename T::template Retype<AN...>; })
            return static_cast<typename T::template Retype<AN...>*>(nullptr);
         else if constexpr (CT::Template<T>) {
            using RETYPER = CT::Inner::IsTemplate<T>;
            return static_cast<typename RETYPER::template Retype<AN...>*>(nullptr);            
         }            
         else return static_cast<T*>(nullptr);
      }
   }

   /// This retype tool will take a (templated or not) T, and subsitute its   
   /// type with another. If `typename T::template Retype` exists, it will be 
   /// used instead. If T is neither templated, nor has a Retype member, then 
   /// T remains unchanged                                                    
   template<class T, class...AN>
   using Retype = Deptr<decltype(Inner::RetypeInner<T, AN...>())>;
}
