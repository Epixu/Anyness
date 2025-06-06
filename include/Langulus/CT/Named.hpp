#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{

   /// Used to define an alternative token for the data definition, as        
   /// opposed to the C++ one                                                 
   /// Types defined with the same token will use the same definition, unless 
   /// they're binary incompatible, in which case a runtime error will occur  
   /// when reflected (if MANAGED_REFLECTION is enabled)                      
   /// Can be used in two ways to satisfy CT::Named<T>:                       
   /// 1. Specialize for T/concept                                            
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

   ///   @important                                                           
   /// When reflecting enums inside your classes/structs, it is recommended   
   /// to use <Langulus/Values.hpp> instead, as it is two levels above this.  
   /// the Named pattern instructs how NameOf works, which is in turn used by 
   /// NamedValues to instruct how such constants are reflected.              

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(Named);

namespace Langulus::CT
{

   /// Check if a constant has a name associate with it                       
   template<auto E>
   concept NamedValue = CTTI::NamedValue<E>::Enabled;

   /// Check if a constant doesn't have a name associate with it              
   template<auto E>
   concept NotNamedValue = not NamedValue<E>;

} // namespace Langulus::CT