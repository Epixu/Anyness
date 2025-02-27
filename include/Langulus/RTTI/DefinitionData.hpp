#pragma once
#include "Definition.hpp"


namespace Langulus::CTTI
{

   /// Everything is considered data by default. This is used to define an    
   /// alternative token for the data definition, as opposed to the C++ one   
   /// Types defined with the same token will use the same definition, unless 
   /// they're binary incompatible, in which case a runtime error will occur  
   /// Can be used in two ways to satisfy CT::Named<T>:                       
   /// 1. Specialize for T/concept having Value as true and an unique name    
   /// 2. Add a public `using CTTI_Named = YesText<"DataID">;` in T           
   template<class T>
   struct Named {
      static constexpr Literal Name = "<will use C++ name>";
      static constexpr bool Value = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Named);

namespace Langulus::RTTI
{

   class MetaData;
   using DMeta = MetaData;
   

   ///                                                                        
   /// A data definition                                                      
   ///                                                                        
   class DefinitionData : public Definition {
      // A sanitized last token (with a lower first letter)             
      ::std::string mTokenSanitized;
   };

} // namespace Langulus::RTTI