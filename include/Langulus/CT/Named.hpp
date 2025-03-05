#pragma once
#include "../CTTI.hpp"


namespace Langulus::CTTI
{

   /// Used to define an alternative token for the data definition, as        
   /// opposed to the C++ one                                                 
   /// Types defined with the same token will use the same definition, unless 
   /// they're binary incompatible, in which case a runtime error will occur  
   /// when reflected (if MANAGED_REFLECTION is enabled)                      
   /// Can be used in two ways to satisfy CT::Named<T>:                       
   /// 1. Specialize for T/concept having Enabled as true and an unique name  
   /// 2. Add a public `using CTTI_Named = YesText<"DataID">;` in T           
   template<class T>
   struct Named {
      static constexpr Literal Name = "<will use C++ name>";
      static constexpr bool Enabled = false;
   };

   template<auto E>
   struct NamedValue {
      static constexpr Literal Name = "<will use C++ name>";
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Named);

namespace Langulus::CT
{
   template<auto E>
   concept NamedValue = CTTI::NamedValue<E>::Enabled;

   template<auto E>
   concept NotNamedValue = not NamedValue<E>;

} // namespace Langulus::CT
