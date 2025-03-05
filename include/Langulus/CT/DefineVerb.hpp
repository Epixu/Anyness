#pragma once
#include "../Literal.hpp"


namespace Langulus
{

   /// Useful for setting CTTI_DefineVerb                                     
   template<Literal POSITIVE, Literal NEGATIVE = "", float PRECEDENCE = 0>
   struct VerbToken {
      static constexpr unsigned Positive = POSITIVE;
      static constexpr unsigned Negative = NEGATIVE;
      static constexpr float Precedence = PRECEDENCE;
      static constexpr bool Enabled = true;
   };

   namespace Anyness
   {
      struct Many;
   }

   namespace Flow
   {
      struct Verb;
   }

} // namespace Langulus

namespace Langulus::CTTI
{

   /// Can be used in three ways to satisfy CT::DefineVerb<T>:                
   /// 1. Specialize for T/concept having Enabled as true and the needed      
   ///    tokens. Negative is optional and makes the verb reversible          
   /// 2. To define a reversible verb add a public                            
   ///   `using CTTI_DefineVerb = VerbToken<"positive", "negative">;` in T    
   /// 3. To define a non-reversible verb add a public                        
   ///   `using CTTI_DefineVerb = VerbToken<"verb">;` in T                    
   template<class T>
   struct DefineVerb {
      static constexpr Literal Positive = "<not a verb>";
      static constexpr Literal Negative = "<not a verb>";
      static constexpr float Precedence = 0;
      static constexpr bool Enabled = false;
   };

   /// Can be used in two ways to satisfy CT::DefineVerbOperator<T>:          
   /// 1. Specialize for T/concept having Enabled as true and the needed      
   ///    tokens. All operators are optional and don't affect reversibility   
   /// 2. To define a verb operator add a public                              
   ///   `using CTTI_DefineVerbOperator = VerbToken<"positive", "negative">;` 
   ///                                 or VerbToken<"positive">;`             
   ///                                 or VerbToken<"negative">;` in T        
   template<class T>
   struct DefineVerbOperator {
      static constexpr Literal Positive = "<not a verb>";
      static constexpr Literal Negative = "<not a verb>";
      static constexpr bool Enabled = false;
   };

} // namespace Langulus::CTTI

LANGULUS_CTTI_CONCEPT(DefineVerb);
LANGULUS_CTTI_CONCEPT(DefineVerbOperator);

namespace Langulus::RTTI
{

   /// Get the name of CTTI_DefineVerb::Positive at compile-time              
   ///   @tparam T - the verb to get the name of                              
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto NameOfVerb() {
      if constexpr (CTTI::DefineVerb<T>::Enabled)
         return CTTI::DefineVerb<T>::Positive;
      else
         return T::CTTI_DefineVerb::Positive;
   }
   
   /// Get the name of CTTI_DefineVerb::Negative at compile-time              
   ///   @tparam T - the verb to get the name of                              
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto NameOfVerbReverse() {
      if constexpr (CTTI::DefineVerb<T>::Enabled)
         return CTTI::DefineVerb<T>::Negative;
      else
         return T::CTTI_DefineVerb::Negative;
   }
   
   /// Get the name of DefineVerbOperator::Positive at compile-time           
   ///   @tparam T - the verb to get the name of                              
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto OperatorOfVerb() {
      if constexpr (CTTI::DefineVerbOperator<T>::Enabled)
         return CTTI::DefineVerbOperator<T>::Positive;
      else if constexpr (requires { T::CTTI_DefineVerbOperator::Enabled; })
         return T::CTTI_DefineVerbOperator::Positive;
      else
         return Literal {""};
   }
   
   /// Get the name of DefineVerbOperator::Negative at compile-time           
   ///   @tparam T - the verb to get the name of                              
   ///   @return the name                                                     
   template<CT::DefineVerb T>
   consteval auto OperatorOfVerbReverse() {
      if constexpr (CTTI::DefineVerbOperator<T>::Enabled)
         return CTTI::DefineVerbOperator<T>::Negative;
      else if constexpr (requires { T::CTTI_DefineVerbOperator::Enabled; })
         return T::CTTI_DefineVerbOperator::Negative;
      else
         return Literal {""};
   }
   
   /// The default verb execution functor                                     
   using FVerbDefaultMutable  = bool (*)(      Anyness::Many&, Flow::Verb&);
   using FVerbDefaultConstant = bool (*)(const Anyness::Many&, Flow::Verb&);
   using FVerbStateless       = bool (*)(Flow::Verb&);

   /// Checks if a verb is default-executable in a mutable context            
   template<CT::DefineVerb T>
   consteval FVerbDefaultMutable VerbDefaultMutable() {
      if constexpr (requires { FVerbDefaultMutable {&T::ExecuteDefault}; })
         return &T::ExecuteDefault;
      else
         return nullptr;
   }

   /// Checks if a verb is default-executable in an immutable context         
   template<CT::DefineVerb T>
   consteval FVerbDefaultConstant VerbDefaultConstant() {
      if constexpr (requires { FVerbDefaultConstant {&T::ExecuteDefault}; })
         return &T::ExecuteDefault;
      else
         return nullptr;
   }

   /// Checks if a verb is stateless-executable                               
   template<CT::DefineVerb T>
   consteval FVerbStateless VerbStateless() {
      if constexpr (requires { FVerbStateless {&T::ExecuteStateless}; })
         return &T::ExecuteStateless;
      else
         return nullptr;
   }

} // namespace Langulus::RTTI
