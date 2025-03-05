#pragma once
#include "../CTTI.hpp"


namespace Langulus
{

   /// Useful for setting CTTI_Version                                        
   template<unsigned MAJOR, unsigned MINOR>
   struct Version {
      static constexpr unsigned Major = MAJOR;
      static constexpr unsigned Minor = MINOR;
      static constexpr bool Enabled = true;
   };

} // namespace Langulus

namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Version<T>:                     
   /// 1. Specialize for T/concept having Enabled as true and a version       
   /// 2. Add a public `using CTTI_Version = Version<major, minor>;` in T     
   template<class T>
   struct Version {
      static constexpr unsigned Major = 1;
      static constexpr unsigned Minor = 0;
      static constexpr bool Enabled = false;
   };

   template<auto E>
   struct VersionValue {
      static constexpr unsigned Major = 1;
      static constexpr unsigned Minor = 0;
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Version);

namespace Langulus::CT
{
   template<auto E>
   concept VersionValue = CTTI::VersionValue<E>::Enabled;

   template<auto E>
   concept NotVersionValue = not VersionValue<E>;

} // namespace Langulus::CT
