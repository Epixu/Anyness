///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <type_traits>
#include <concepts>


namespace Langulus
{

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

      template<class T>
      consteval auto NestedDecay() {
         using Stripped = Decvq<Deptr<Deext<T>>>;
         if constexpr (::std::same_as<T, Stripped>)
            return static_cast<Stripped*>(nullptr);
         else
            return NestedDecay<Stripped>();
      }

   } // namespace Langulus::Inner

   /// Strip a typename to its origin type, removing qualifiers/pointers/etc. 
   /// This strongly guarantees, that it strips EVERYTHING, including nested  
   /// pointers, extents, etc.                                                
   template<class T>
   using Decay = Deptr<decltype(Inner::NestedDecay<T>())>;

} // namespace Langulus