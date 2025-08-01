///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus
{
   /// Useful for setting CTTI_Versioned                                      
   template<unsigned MAJOR, unsigned MINOR>
   struct Version {
      static constexpr unsigned Major = MAJOR;
      static constexpr unsigned Minor = MINOR;
      static constexpr bool Enabled = true;
   };
}

namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Versioned<T>:                   
   /// 1. Specialize for T/concept having Enabled as true and a version       
   /// 2. Add a public `using CTTI_Versioned = Version<major, minor>;` in T   
   template<class T>
   struct Versioned;

   template<auto E>
   struct VersionedValue;
}

LANGULUS_CTTI_CONCEPT_DECVQ(Versioned);

namespace Langulus::CT
{
   template<auto E>
   concept VersionedValue = Complete<CTTI::VersionedValue<E>>;

   template<auto E>
   concept NotVersionedValue = not Complete<CTTI::VersionedValue<E>>;
}

namespace Langulus
{
   ///                                                                        
   template<class T>
   consteval auto VersionOf() {
      using ST = Shed<T>;
      if constexpr (CT::Complete<CTTI::Versioned<ST>>)
         return CTTI::Versioned<ST> {};
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, Versioned))
         return typename Decay<ST>::CTTI_Versioned {};
      else
         return Version<1, 0> {};
   }

   ///                                                                        
   template<auto E>
   consteval auto VersionOf() {
      if constexpr (CT::Complete<CTTI::VersionedValue<E>>)
         return CTTI::VersionedValue<E> {};
      else
         return Version<1, 0> {};
   }
}
