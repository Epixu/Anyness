///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Definition.hpp"


namespace Langulus::RTTI::Inner
{
   
   /// Construct an abstract definition                                       
   ///   @param cppname - the name of the definition, as it appears in C++    
   LANGULUS(ALWAYS_INLINED)
   Definition::Definition(const Token& cppname)
      : mHash    {HashOf(cppname)}
      , mCppName {cppname} {}

   /// Reflect some common type properties, like C++ name, info and version   
   ///   @tparam T - the type to reflect                                      
   template<CT::Decayed T>
   void Definition::ReflectCommon() {
      if constexpr (CT::Info<T>) {
         // Reflected info                                              
         if constexpr (CTTI::Info<T>::Enabled)
            mInfo = CTTI::Info<T>::Text;
         else if constexpr (requires { T::CTTI_Info::Enabled; })
            mInfo = T::CTTI_Info::Constant;
      }

      if constexpr (CT::Version<T>) {
         // Reflected version                                           
         if constexpr (CTTI::Version<T>::Enabled) {
            mVersionMajor = CTTI::Version<T>::Major;
            mVersionMinor = CTTI::Version<T>::Minor;
         }
         else if constexpr (requires { T::CTTI_Version::Enabled; }) {
            mVersionMajor = T::CTTI_Version::Constant::Major;
            mVersionMinor = T::CTTI_Version::Constant::Minor;
         }
      }
   }
      
} // namespace Langulus::RTTI::Inner
