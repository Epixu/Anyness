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
   struct Versioned {
      static constexpr unsigned Major = 1;
      static constexpr unsigned Minor = 0;
      static constexpr bool Enabled = false;
   };

   template<auto E>
   struct VersionedValue {
      static constexpr unsigned Major = 1;
      static constexpr unsigned Minor = 0;
      static constexpr bool Enabled = false;
   };
}

LANGULUS_CTTI_CONCEPT(Versioned);

namespace Langulus::CT
{
   template<auto E>
   concept VersionedValue = CTTI::VersionedValue<E>::Enabled;

   template<auto E>
   concept NotVersionedValue = not VersionedValue<E>;
}

namespace Langulus
{
   ///                                                                        
   template<class T>
   consteval auto VersionOf() {
      using ST = Shed<T>;
      if constexpr (requires { CTTI::Versioned<ST>::Enabled; })
         return CTTI::Versioned<ST> {};
      else if constexpr (LANGULUS_CTTI_DELVE_IN(ST, Versioned))
         return typename Decay<ST>::CTTI_Pooled {};
      else
         return CTTI::Versioned<void> {};
   }

   ///                                                                        
   template<auto E>
   consteval auto VersionOf() {
      if constexpr (requires { CTTI::VersionedValue<E>::Enabled; })
         return CTTI::VersionedValue<E> {};
      else
         return CTTI::VersionedValue<0> {};
   }
}
