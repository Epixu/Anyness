#pragma once
#include "Definition.hpp"


namespace Langulus::CTTI
{
   
   /// Can be used in two ways to satisfy CT::DefineConstant<T>:              
   /// 1. Specialize for T/concept having Value as true and a unique Name     
   /// 2. Add a public `using CTTI_DefineConstant = YesText<"ConstID">;` in T 
   template<class T>
   struct DefineConstant {
      static constexpr Literal Name = "<not a constant>";
      static constexpr bool Value = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(DefineConstant);

namespace Langulus::RTTI
{

   class MetaConst;
   using CMeta = MetaConst;
   

   ///                                                                        
   /// A constant definition                                                  
   ///                                                                        
   class DefinitionConst : public Definition {
      // A sanitized last token (with a lower first letter)             
      ::std::string mTokenSanitized;
   };

} // namespace Langulus::RTTI