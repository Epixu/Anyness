///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Typenav.hpp"
#include "Literal.hpp"
#include "CT/Info.hpp"


namespace Langulus
{
   /// Get the info for a type at compile-time                                
   ///   @tparam T - the type to get the info of                              
   ///   @return a compile-time string                                        
   template<class T>
   consteval auto InfoOf() {
      using DT = Decvq<Deref<T>>;
      
      if constexpr (CTTI::Info<DT>::Enabled)
         return CTTI::Info<DT>::Text;
      else if constexpr (::std::is_class_v<DT>) {
         if constexpr (requires { DT::CTTI_Info::Constant; })
            return DT::CTTI_Info::Constant;
         else return Literal {};
      }
      else return Literal {};
   }
   
   /// Get the info for a constant at compile-time                            
   ///   @tparam E - the constant to get the info of                          
   ///   @return a compile-time string                                        
   template<auto E>
   consteval auto InfoOf() {
      if constexpr (CTTI::InfoValue<E>::Enabled)
         return CTTI::InfoValue<E>::Enabled;
      else
         return Literal {};
   }
}
