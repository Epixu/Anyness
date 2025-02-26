#pragma once
#include <type_traits>


namespace Langulus
{

   /// Equivalent to ::std::true_type, but without the silly nomenclature     
   struct Yes {
      static constexpr bool Value = true;
   };

   /// Equivalent to Yes, but also carries a constant                         
   template<auto CONSTANT>
   struct YesVal {
      static constexpr auto Constant = CONSTANT;
      static constexpr bool Value = true;
   };

   /// Equivalent to ::std::false_type, but without the silly nomenclature    
   struct No {
      static constexpr bool Value = false;
   };

} // namespace Langulus


/// A namespace for defining compile-time type information tags               
/// Specializing <type_traits> is generally undefined behavior, but here      
/// we have alternatives that are more flexible, using type_traits as the     
/// ground truth and building on top of on them.                              
/// Read more: https://stackoverflow.com/questions/25345486                   
namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Void<T>:                        
   /// 1. Specialize for T/concept with Value == true                         
   /// 2. Add a public `using CTTI_Void = Yes;` in T                          
   template<class T>
   struct Void {
      static constexpr bool Value = ::std::is_void_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Array<T>:                       
   /// 1. Specialize for T/concept with Value == true                         
   /// 2. Add a public `using CTTI_Array = Yes;` in T                         
   template<class T>
   struct Array {
      static constexpr bool Value = ::std::is_bounded_array_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Enum<T>:                        
   /// 1. Specialize for T/concept with Value == true                         
   /// 2. Add a public `using CTTI_Enum = Yes;` in T                          
   template<class T>
   struct Enum {
      static constexpr bool Value = ::std::is_enum_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Sparse<T>:                      
   /// 1. Specialize for T/concept with Value == true                         
   /// 2. Add a public `using CTTI_Sparse = Yes;` in T                        
   template<class T>
   struct Sparse {
      static constexpr bool Value = ::std::is_pointer_v<T> or Array<T>::Value;
   };

   /// Can be used in two ways to satisfy CT::Typed<T>:                       
   /// 1. Specialize for T/concept having non-void Type                       
   /// 2. Add a public `using CTTI_Typed = non_void_type;` in T               
   template<class T>
   struct Typed {
      using Type = void;
   };

   /// Can be used in two ways to satisfy CT::Typelist<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Typelist = Yes;` in T                      
   template<class T>
   struct Typelist {
      static constexpr bool Value = false;
   };

} // namespace Langulus::CTTI