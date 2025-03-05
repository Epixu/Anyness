#pragma once
#include <type_traits>
#include "Literal.hpp"


namespace Langulus
{

   /// Equivalent to ::std::true_type, but without the silly nomenclature     
   struct Yes {
      static constexpr bool Enabled = true;
   };

   /// Equivalent to Yes, but also carries a string literal                   
   template<Literal TEXT>
   struct YesText {
      static constexpr Literal Constant = TEXT;
      static constexpr bool Enabled = true;
   };

   /// Equivalent to Yes, but also carries a constant of any type             
   template<auto VALUE>
   struct YesValue {
      static constexpr auto Constant = VALUE;
      static constexpr bool Enabled = true;
   };

   /// Equivalent to ::std::false_type, but without the silly nomenclature    
   struct No {
      static constexpr bool Enabled = false;
   };

} // namespace Langulus


/// A namespace for defining compile-time type information tags               
/// Specializing <type_traits> is generally undefined behavior, but here      
/// we have alternatives that are more flexible, using type_traits as the     
/// ground truth and building on top of on them                               
/// Read more: https://stackoverflow.com/questions/25345486                   
namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Void<T>:                        
   /// 1. Specialize for T/concept with Value == true                         
   /// 2. Add a public `using CTTI_Void = Yes;` in T                          
   template<class T>
   struct Void {
      static constexpr bool Enabled = ::std::is_void_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Enum<T>:                        
   /// 1. Specialize for T/concept with Value == true                         
   /// 2. Add a public `using CTTI_Enum = Yes;` in T                          
   template<class T>
   struct Enum {
      static constexpr bool Enabled = ::std::is_enum_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Aggregate<T>:                   
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Aggregate = Yes;` in T                     
   template<class T>
   struct Aggregate {
      static constexpr bool Enabled = ::std::is_aggregate_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Fundamental<T>:                 
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Fundamental = Yes;` in T                   
   template<class T>
   struct Fundamental {
      static constexpr bool Enabled = ::std::is_fundamental_v<T>;
   };

} // namespace Langulus::CTTI

#define LANGULUS_CTTI_CONCEPT(NAME) \
   namespace Langulus::CT { \
      template<class...T> \
      concept NAME = ((CTTI::NAME<T>::Enabled or T::CTTI_##NAME::Enabled) and ...); \
      template<class...T> \
      concept Not##NAME = ((not NAME<T>) and ...); \
   }

LANGULUS_CTTI_CONCEPT(Void);
LANGULUS_CTTI_CONCEPT(Enum);
LANGULUS_CTTI_CONCEPT(Aggregate);
LANGULUS_CTTI_CONCEPT(Fundamental);

namespace Langulus::CT
{

   /// Check if all T are function signatures                                 
   ///   @attention std::function, lambdas, classes with overloaded           
   ///      operator() and pointers to functions don't count as function types
   template<class...T>
   concept Function = (::std::is_function_v<T> and ...);

} // namespace Langulus::CT