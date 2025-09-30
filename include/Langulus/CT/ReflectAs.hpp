///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Typenav.hpp"


namespace Langulus::CTTI
{
   /// Can be used in two ways to satisfy CT::ReflectAs<T>:                   
   /// 1. Specialize for T/concept, with the desired Type                     
   /// 2. Add a public `using CTTI_ReflectAs = <DesiredType>;` in T           
   /// Use void/No type to disable reflection for the type                    
   template<class T>
   struct ReflectAs;

   /// nullptr_t is not reflectable                                           
   template<>
   struct ReflectAs<nullptr_t> {
      using Type = void;
   };
}

namespace Langulus::CT
{
   namespace Inner
   {
      /// Convenience function that checks if ReflectAs is not void, which    
      /// would mean that the type is not reflectable at all. It also makes   
      /// sure that if ReflectAs is specified, then the involved types are    
      /// binary-compatible.                                                  
      template<class T>
      consteval auto IsReflectable() {
         static_assert(NotReference<T>, "Strip references first");
         static_assert(NotSheddable<T>, "Strip sheddable types first");
         using DT = Decay<T>;

         if constexpr (Void<T>) {
            // Void types are never reflectable                         
            return static_cast<void*>(nullptr);
         }
         else if constexpr (Complete<CTTI::ReflectAs<T>>) {
            // Substitution through external template                   
            // Despite this, all participating types must be complete   
            // because their `sizeof` and `alignof` are checked.        
            using AS = typename CTTI::ReflectAs<T>::Type;
            if constexpr (Void<AS>)
               return static_cast<void*>(nullptr);
            else {
               static_assert(sizeof(T) == sizeof(AS),
                  "Provided ReflectAs type must be of the same size");
               static_assert(alignof(T) == alignof(AS),
                  "Provided ReflectAs type must be of the same alignment");
               return static_cast<AS*>(nullptr);
            }
         }
         else if constexpr (Dense<T> and requires { typename DT::CTTI_ReflectAs; }) {
            // Substitution through internal type                       
            using AS = typename DT::CTTI_ReflectAs;
            if constexpr (Void<AS>)
               return static_cast<void*>(nullptr);
            else {
               static_assert(sizeof(T) == sizeof(AS),
                  "Provided ReflectAs type must be of the same size");
               static_assert(alignof(T) == alignof(AS),
                  "Provided ReflectAs type must be of the same alignment");
               return static_cast<AS*>(nullptr);
            }
         }
         else return static_cast<T*>(nullptr);
      }
   }

   /// Check if all T are reflectable                                         
   template<class...T>
   concept Reflectable = Validate<T...>
       and (CT::NotVoid<Deptr<decltype(Inner::IsReflectable<T>())>> and ...);

   /// Get the type a given T is reflected as. This is very useful as a       
   /// a build-time optimization, because many type-erased containers are     
   /// binary-compatible with their templated equivalents, and the use of     
   /// CTTI_ReflectAs can drastically lower build time for meta generation,   
   /// by reducing unnessesary template reflections of redundant types.       
   ///   @attention this is designed only for affecting the reflection of     
   ///      data types, not tag, verb, or constant definitions                
   template<class T>
   using ReflectedAs = Deptr<decltype(Inner::IsReflectable<T>())>;
}
