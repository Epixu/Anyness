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
            return Types<typename T::template Retype<AN...>> {};
         else if constexpr (CT::Template<T>) {
            using RETYPER = CT::Inner::IsTemplate<T>;
            return Types<typename RETYPER::template Retype<AN...>> {};
         }            
         else return Types<T> {};
      }
   }

   /// This retype tool will take a (templated or not) T, and subsitute its   
   /// type with another. If `typename T::template Retype` exists, it will be 
   /// used instead. If T is neither templated, nor has a Retype member, then 
   /// T remains unchanged                                                    
   template<class T, class...AN>
   using Retype = typename decltype(Inner::RetypeInner<T, AN...>())::First;
}
