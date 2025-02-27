#pragma once
#include "Definition.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::DefineTrait<T>:                 
   /// 1. Specialize for T/concept having Value as true and a unique Name     
   /// 2. Add a public `using CTTI_DefineTrait = YesText<"TraitID">;` in T    
   template<class T>
   struct DefineTrait {
      static constexpr Literal Name = "<not a trait>";
      static constexpr bool Value = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(DefineTrait);

namespace Langulus::RTTI
{

   class MetaTrait;
   using TMeta = MetaTrait;
   

   ///                                                                        
   /// A trait definition                                                     
   ///                                                                        
   class DefinitionTrait : public Definition {
      // A sanitized last token (with a lower first letter)             
      ::std::string mTokenSanitized;

   public:
      DefinitionTrait(const Token& token) : Definition {token} {
         mTokenSanitized = ToLastToken(mToken);
         mTokenSanitized[0] = ::std::tolower(mTokenSanitized[0]);
      }
   };

} // namespace Langulus::RTTI