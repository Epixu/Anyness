///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <concepts>


/// A namespace for defining compile-time type information tags               
/// Specializing <type_traits> is generally undefined behavior, but here      
/// we have alternatives that are more flexible, using type_traits as the     
/// ground truth and building on top of on them                               
/// Read more: https://stackoverflow.com/questions/25345486                   
namespace Langulus::CTTI
{
      
   /// Can be used in two ways to satisfy CT::Array<T>:                       
   /// 1. Specialize for T/concept with Value == true                         
   /// 2. Add a public `using CTTI_Array = Yes;` in T                         
   template<class T>
   struct Array {
      static constexpr bool Enabled = ::std::is_bounded_array_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Sparse<T>:                      
   /// 1. Specialize for T/concept with Value == true                         
   /// 2. Add a public `using CTTI_Sparse = Yes;` in T                        
   template<class T>
   struct Sparse {
      static constexpr bool Enabled = ::std::is_pointer_v<T>;
   };

   /// Can be used in two ways to satisfy CT::Constant<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Constant = Yes;` in T                      
   template<class T>
   struct Constant {
      static constexpr bool Enabled = ::std::is_const_v<T>;
   };
   
   /// Can be used in two ways to satisfy CT::Volatile<T>:                    
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Volatile = Yes;` in T                      
   template<class T>
   struct Volatile {
      static constexpr bool Enabled = ::std::is_volatile_v<T>;
   };
   
} // namespace Langulus::CTTI

namespace Langulus
{

   /// Get the extent of an array type, or 1 if T is not an array             
   template<class T>
   constexpr ::std::size_t ExtentOf = ::std::is_bounded_array_v<T> ? ::std::extent_v<T> : 1;

   /// Get the extent of an array argument, or 1 if T is not an array         
   template<class T>
   consteval ::std::size_t GetExtentOf(T&&) { return ExtentOf<T>; }

   /// Same as ::std::declval, but more humanely named                        
   template<class T>
   ::std::add_rvalue_reference_t<T> Fake() noexcept {
      static_assert(false, "Calling Fake is ill-formed");
   }

   /// Same as ::std::declval, but deduces type via argument                  
   template<class T>
   ::std::add_rvalue_reference_t<T> Fake(T) noexcept {
      static_assert(false, "Calling Fake is ill-formed");
   }

   /// Remove a reference from type                                           
   template<class T>
   using Deref = ::std::remove_reference_t<T>;

   /// Remove a pointer from type                                             
   ///   @attention a type can still be CT::Sparse after being Deptr,         
   ///      when using custom packed pointer types for example. Deptr         
   ///      removes only indirections that are part of the C++ syntax.        
   template<class T>
   using Deptr = ::std::remove_pointer_t<T>;

   /// Remove a const/volatile from a type                                    
   template<class T>
   using Decvq = ::std::remove_cv_t<T>;

   /// Remove a const from a type                                             
   template<class T>
   using Decq = ::std::remove_const_t<T>;

   /// Remove a volatile from a type                                          
   template<class T>
   using Devq = ::std::remove_volatile_t<T>;

   /// Remove an array extent from a type                                     
   template<class T>
   using Deext = ::std::remove_extent_t<Deref<T>>;
   
   namespace Inner
   {

      /// Nest-strip any qualifiers, extents and indirections                 
      /// Returns a pointer to the stripped T                                 
      template<class T>
      consteval auto NestedDecay() {
         using Stripped = Decvq<Deptr<Deext<T>>>;
         if constexpr (::std::same_as<T, Stripped>)
            return static_cast<Stripped*>(nullptr);
         else
            return NestedDecay<Stripped>();
      }

   } // namespace Langulus::Inner

   /// Strip a typename to its identity, removing qualifiers/pointers/etc.    
   /// This strongly guarantees, that it strips EVERYTHING, including nested  
   /// pointers and extents                                                   
   template<class T>
   using Decay = Deptr<decltype(Inner::NestedDecay<T>())>;
   
   namespace CT
   {

      /// Check if all T are bounded arrays                                   
      template<class...T>
      concept Array = ((CTTI::Array<T>::Enabled or T::CTTI_Array::Enabled) and ...);

      /// Check if all T are volatile-qualified                               
      template<class...T>
      concept Volatile = ((CTTI::Volatile<T>::Enabled or T::CTTI_Volatile::Enabled) and ...);

      /// Check if all T are sparse                                           
      ///   @attention this also includes non-pointer types that are tagged   
      ///      as custom packed pointers                                      
      template<class...T>
      concept Sparse = ((CTTI::Sparse<T>::Enabled or T::CTTI_Sparse::Enabled) and ...);

      /// Check if all T are dense                                            
      template<class...T>
      concept Dense = ((not Sparse<T>) and ...);

      /// Check if all T are constant-qualified                               
      template<class...T>
      concept Constant = ((CTTI::Constant<Deref<T>>::Enabled or Deref<T>::CTTI_Constant::Enabled) and ...);

      /// Check if all T are not constant-qualified                           
      template<class...T>
      concept Mutable = ((not Constant<Deref<T>>) and ...);

      /// Check if all T are either const- and/or volatile-qualified          
      template<class...T>
      concept Convoluted = ((Constant<T> or Volatile<T>) and ...);

      /// Check if none of T are const- and/or volatile-qualified             
      template<class...T>
      concept NotConvoluted = ((not Convoluted<T>) and ...);

      /// Check if all T are reference types                                  
      template<class...T>
      concept Reference = (::std::is_reference_v<T> and ...);

      /// Check if all T are not reference types                              
      template<class...T>
      concept NotReference = ((not Reference<T>) and ...);

      /// Check if types have no reference/pointer/extent/qualifiers          
      ///   @attention a type can still be CT::Sparse while being CT::Decayed,
      ///      when using custom packed pointer types for example. Decaying   
      ///      removes only indirections that are part of the C++ syntax      
      template<class...T>
      concept Decayed = ((not ::std::is_bounded_array_v<T>
          and not ::std::is_pointer_v<T>
          and not Reference<T>
          and not Convoluted<T>
        ) and ...);
   
      /// Check if types have reference/pointer/extent/const/volatile         
      template<class...T>
      concept NotDecayed = ((not Decayed<T>) and ...);

      /// True if T is not a pointer, has no extent with [], and isn't a      
      /// reference. T is still allowed to be cv-qualified                    
      template<class...T>
      concept Slab = ((Dense<T> and not Reference<T>) and ...);

   } // namespace Langulus::CT


   /// I don't like how long ::std::conditional_t is to write                 
   template<bool CONDITION, class YES, class NO>
   using Tif = ::std::conditional_t<CONDITION, YES, NO>;

   
   /// Always returns a pointer to the argument                               
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr decltype(auto) SparseCast(T&& a) noexcept {
      if constexpr (CT::Sparse<Deref<T>>) return (a);
      else return &a;
   }

   /// Always returns a value reference to the argument                       
   /// If argument is an array, return a value reference to the first element 
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr decltype(auto) DenseCast(T&& a) {
      if constexpr (CT::Array<Deref<T>>)
         return DenseCast(a[0]);
      else if constexpr (CT::Sparse<Deref<T>>) {
         if (a == nullptr)
            throw Exception("Can't dereference nullptr");
         return DenseCast(*a);
      }
      else return (a);
   }

} // namespace Langulus