#pragma once
#include "Definition.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::DefineVerb<T>:                  
   /// Used to define a verb without an antonym (not reversible)              
   /// 1. Specialize for T/concept having Value as true and a unique Name     
   /// 2. Add a public `using CTTI_DefineVerb = YesText<"VerbID">;` in T      
   template<class T>
   struct DefineVerb {
      static constexpr Literal Name = "<not a verb>";
      static constexpr bool Value = false;
   };
   
   /// Can be used in two ways to satisfy CT::DefinePositiveVerb<T>:          
   /// Used to define positive token for a verb with an antonym (reversible)  
   /// 1. Specialize for T/concept having Value as true and a unique Name     
   /// 2. Add `using CTTI_DefinePositiveVerb = YesText<"PositiveID">;` in T   
   template<class T>
   struct DefinePositiveVerb {
      static constexpr Literal Name = "<not a verb>";
      static constexpr bool Value = false;
   };
   
   /// Can be used in two ways to satisfy CT::DefineNegativeVerb<T>:          
   /// Used to define the antonym for a verb (the reversing function)         
   /// 1. Specialize for T/concept having Value as true and a unique Name     
   /// 2. Add `using CTTI_DefineNegativeVerb = YesText<"MegativeID">;` in T   
   template<class T>
   struct DefineNegativeVerb {
      static constexpr Literal Name = "<not a verb>";
      static constexpr bool Value = false;
   };

} // namespace Langulus::CTTI

namespace Langulus::CT
{

   template<class...T>
   concept ReversibleVerb = ((
           (CTTI::DefinePositiveVerb<T>::Value or T::CTTI_DefinePositiveVerb::Value)
       and (CTTI::DefineNegativeVerb<T>::Value or T::CTTI_DefineNegativeVerb::Value)
     ) and ...);

   template<class...T>
   concept DefineVerb = ((
           (CTTI::DefineVerb<T>::Value or T::CTTI_DefineVerb::Value)
        or ReversibleVerb<T>
     ) and ...);

} // namespace Langulus::CT

namespace Langulus::RTTI
{

   class MetaVerb;
   using VMeta = MetaVerb;
   

   ///                                                                        
   /// A trait definition                                                     
   ///                                                                        
   class DefinitionVerb : public Definition {
      // A sanitized last token (with a lower first letter)             
      ::std::string mTokenSanitized;

   public:
      DefinitionVerb(const Token& token) : Definition {token} {
         mTokenSanitized = ToLastToken(mToken);
         mTokenSanitized[0] = ::std::tolower(mTokenSanitized[0]);
      }
   };

} // namespace Langulus::RTTI