///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Info<T>:                        
   /// 1. Specialize for T/concept having Enabled as true and an info string  
   /// 2. Add a public `using CTTI_Info = Yes<"some info">;` in T             
   template<class T>
   struct Info {
      static constexpr Literal Text = {};
      static constexpr bool Enabled = false;
   };

   template<auto E>
   struct InfoValue {
      static constexpr Literal Text = {};
      static constexpr bool Enabled = false;
   };
}

LANGULUS_CTTI_CONCEPT(Info);

namespace Langulus::CT
{
   template<auto E>
   concept InfoValue = CTTI::InfoValue<E>::Enabled;

   template<auto E>
   concept NotInfoValue = not InfoValue<E>;
}

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
         else
            return Literal {};
      }
      else return Literal {};
   }
   
   /// Get the info for a constant at compile-time                            
   ///   @tparam E - the constant to get the info of                          
   ///   @return a compile-time string                                        
   template<auto E>
   consteval auto InfoOf() {
      if constexpr (CTTI::InfoValue<E>::Enabled)
         return CTTI::InfoValue<E>::Text;
      else
         return Literal {};
   }
}
