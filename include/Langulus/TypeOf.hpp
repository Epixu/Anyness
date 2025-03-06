///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "TypeNav.hpp"


namespace Langulus::CT::Inner
{

   template<class T>
   consteval bool IsTyped() {
      if constexpr (NotVoid<typename CTTI::Typed<T>::Type>)
         // Checked externally, T doesn't have to be complete           
         return true;
      else if constexpr (requires { typename T::CTTI_Typed; })
         // Checked internally, T has to be a complete type             
         return NotVoid<typename T::CTTI_Typed>;
      else if constexpr (requires { typename T::value_type; })
         // Checked internally, T has to be a complete type             
         return NotVoid<typename T::value_type>;
      else
         return false;
   }

   template<class T>
   consteval CT::Typelist auto GetUnderlyingType() {
      if constexpr (Array<T>)
         return Types<Deext<T>> {};
      else {
         if constexpr (NotVoid<typename CTTI::Typed<T>::Type>)
            // Checked externally, T doesn't have to be complete        
            return Types<typename CTTI::Typed<T>::Type> {};
         else if constexpr (requires { typename T::CTTI_Typed; })
            // Checked internally, T has to be a complete type          
            return Types<typename T::CTTI_Typed> {};
         else if constexpr (requires { typename T::value_type; })
            // Checked internally, T has to be a complete type          
            return Types<typename T::value_type> {};
         else if constexpr (Enum<T>)
            return Types<::std::underlying_type_t<T>> {};
         else
            return Types<T> {};
      }
   };

} // namespace Langulus::CT::Inner

namespace Langulus
{

   /// Get the type that wraps std::underlying_type_t<T> for enums,           
   /// as well as any bounded array, or anything with T::CTTI_Typed or        
   /// T::value_type that isn't 'void'                                        
   ///   - if T is an array -> return the type (remove extents and refs)      
   ///   - if T has CTTI_Typed/value_type -> return the inner type            
   ///   - if T is an enum -> return the underlying type                      
   ///   - otherwise just return T                                            
   template<class T>
   using TypeOf = typename decltype(CT::Inner::GetUnderlyingType<T>())::First;

   namespace CT
   {

      /// Check if all T are typed by searching for CTTI::Typed<T>            
      /// specializations, or T::CTTI_Typed / T::value_type members           
      ///   @attention the inner type must not be 'void', in order for T to   
      ///      be considered 'typed' (as opposed to 'type-erased')            
      template<class...T>
      concept Typed = (Inner::IsTyped<T>() and ...);

      /// Check if all T have no underlying types defined                     
      template<class...T>
      concept Untyped = ((not Typed<T>) and ...);

   } // namespace Langulus::CT


   /// Downcasts a typed wrapper to the contained element, if cast operator   
   /// to TypeOf<T> is available                                              
   ///  - if T isn't typed, just return itself                                
   ///  - otherwise:                                                          
   ///      -      if T::TypedCast() exists use that                          
   ///      - else if T::operator TypeOf<T>&& exists use that                 
   ///      - else if T::operator TypeOf<T>& exists use that                  
   ///      - else if T::operator const TypeOf<T>& exists use that            
   ///      - else if T::operator TypeOf<T> exists use that                   
   ///   @param what - the instance to decay                                  
   ///   @return a reference to the the inner data                            
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr decltype(auto) TypedCast(T&& what) {
      if constexpr (CT::Typed<T>) {
         using TT = TypeOf<T>;

         if constexpr (requires { what.TypedCast(); })
            return what.TypedCast();
         else if constexpr (requires { what.operator TT&& (); })
            return what.operator TT&& ();
         else if constexpr (requires { what.operator TT& (); })
            return what.operator TT& ();
         else if constexpr (requires { what.operator const TT& (); })
            return what.operator const TT& ();
         else if constexpr (requires { what.operator TT (); })
            return what.operator TT ();
         else
            static_assert(false, "No cast operator available for decaying to inner type");
      }
      else return what;
   }

} // namespace Langulus
