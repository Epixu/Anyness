#pragma once
#include "../Literal.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::DefineTrait<T>:                 
   /// 1. Specialize for T/concept having Value as true and a unique Name     
   /// 2. Add a public `using CTTI_DefineTrait = YesText<"TraitID">;` in T    
   template<class T>
   struct DefineTrait {
      static constexpr Literal Name = "<not a trait>";
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(DefineTrait);

namespace Langulus::RTTI
{

   /// Get the name of a trait definition at compile-time                     
   ///   @tparam T - the trait to get the name of                             
   ///   @return the name                                                     
   template<CT::DefineTrait T>
   consteval auto NameOfTrait() {
      if constexpr (CTTI::DefineTrait<T>::Enabled)
         return CTTI::DefineTrait<T>::Name;
      else if constexpr (requires { T::CTTI_DefineTrait::Enabled; })
         return T::CTTI_DefineTrait::Constant;
      else
         return Literal {""};
   }

} // namespace Langulus::RTTI
