///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Typenav.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::Typed<T>:                       
   /// 1. Specialize for T/concept having non-void Type                       
   /// 2. Add a public `using CTTI_Typed = <non void type/typelist>;` in T    
   template<class T>
   struct Typed;
}

namespace Langulus::CT::Inner
{
   /// Helper function to extract underlying type                             
   template<class T>
   consteval auto GetUnderlyingType() {
      static_assert(not ::std::is_reference_v<T>, "Strip references first");

      if constexpr (::std::is_bounded_array_v<T>)
         return Types<Deext<T>> {};
      else if constexpr (Complete<CTTI::Typed<T>>) {
         // Checked externally, T doesn't have to be complete           
         return Types<typename CTTI::Typed<T>::Type> {};
      }
      else if constexpr (::std::is_enum_v<T>)
         return Types<::std::underlying_type_t<T>> {};
      else if constexpr (::std::is_class_v<T>) {
         // Checked internally, T has to be a complete type             
         static_assert(Complete<T>,
            "Can't get inner type of an incomplete outer type");
         if constexpr (requires { typename T::CTTI_Typed; }) {
            using InnerT = typename T::CTTI_Typed;
            if constexpr (::std::is_void_v<InnerT> or ::std::same_as<InnerT, No>)
               return NoTypes {};
            else {
               static_assert(not ::std::same_as<InnerT, Yes<>>,
                  "Instead of Yes<> pick a type for CTTI_Typed");
               return Types<InnerT> {};
            }
         }
         else if constexpr (requires { typename T::value_type; })
            return Types<typename T::value_type> {};
         else
            return NoTypes {};
      }
      else return NoTypes {};
   };
}

namespace Langulus
{
   /// Get the type that wraps std::underlying_type_t<T> for enums,           
   /// as well as any bounded array, or anything with CTTI::Typed::Type or    
   /// T::CTTI_Typed/T::value_type that isn't 'void'. Will result in a type   
   /// list if inner type contains more than one type.                        
   ///   - if T is an array -> return the type (remove extents and refs)      
   ///   - if T has CTTI::Typed is specialized -> return CTTI::Typed::Type    
   ///   - if T is an enum -> return the underlying type                      
   ///   - if T has CTTI_Typed/value_type -> return the inner type(s)         
   ///   - otherwise just return a void type                                  
   template<class T>
   using TypeOf = typename decltype(CT::Inner::GetUnderlyingType<Deref<T>>())::First;

   namespace CT
   {
      /// Check if all T are typed by searching for CTTI::Typed<T>            
      /// specializations, or T::CTTI_Typed / T::value_type members.          
      ///   @attention the inner type must not be 'void', in order for T to   
      ///      be considered 'typed' (as opposed to 'type-erased')            
      ///   @attention if the inner type is a typelist, that typelist will be 
      ///      accounted for, and Ts are multiply-typed (like TPair)          
      template<class...T>
      concept Typed = PartialValidate<T...> and (NotVoid<TypeOf<Deref<T>>> and ...);

      /// Check if all T have no underlying types defined                     
      template<class...T>
      concept Untyped = PartialValidate<T...> and (Void<TypeOf<Deref<T>>> and ...);
   }

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
   ///   @return the inner data                                               
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr decltype(auto) TypedCast(T&& what) {
      if constexpr (requires { what.TypedCast(); }) {
         using InnerT = decltype(what.TypedCast());
         static_assert(not ::std::same_as<Decvq<Deref<T>>, Decvq<Deref<InnerT>>>,
            "TypedCast() returns the same type, and will result in infinite regress");
         return what.TypedCast();
      }
      else {
         using InnerT = TypeOf<T>;
         static_assert(not ::std::same_as<Decvq<Deref<T>>, Decvq<Deref<InnerT>>>,
            "TypeOf returns the same type, and will result in infinite regress");

         if constexpr (CT::Void<InnerT>)
            return FWD(what);
         else if constexpr (requires { what.operator InnerT (); })
            return what.operator InnerT ();
         else {
            static_assert(false,
               "No cast operator or TypedCast method available for casting to inner type. "
               "Check the constness of your TypedCast() or cast operators? "
               "Is it compatible with the contained type?"
            );
         }
      }
   }

   /// Strips all sheddable layers down to the first non-sheddable inner type 
   template<class T>
   constexpr decltype(auto) ShedCast(T&& item) noexcept {
      if constexpr (CT::Sheddable<T>)
         return ShedCast(TypedCast(FWD(item)));
      else
         return FWD(item);
   };
   
   /// Always returns a pointer to the argument                               
   template<class T>
   constexpr decltype(auto) SparseCast(T&& a) noexcept {
      if constexpr (::std::is_pointer_v<Shed<T>>)
         return  ShedCast(FWD(a));
      else
         return &ShedCast(FWD(a));
   }

   /// Always returns a value reference to the argument                       
   /// If argument is an array, return a value reference to the first element 
   template<class T>
   constexpr decltype(auto) DenseCast(T&& a) {
      if constexpr (CT::Array<Shed<T>>)
         return DenseCast(ShedCast(FWD(a))[0]);
      else if constexpr (CT::Sparse<Shed<T>>)
         // Security is on you - call can throw                         
         return DenseCast(*ShedCast(FWD(a)));
      else
         return ShedCast(FWD(a));
   }
}
