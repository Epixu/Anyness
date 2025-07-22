///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "../Types.hpp"


namespace Langulus::CT::Inner
{
   template<class>
   struct IsTemplate {
      static constexpr bool Value = false;
      static constexpr size_t ArgumentCount = 0;
      using Arguments = void;
   };

   template<template<class...> class T, class...ARGS>
   struct IsTemplate<T<ARGS...>> {
      static constexpr bool Value = true;
      static constexpr size_t ArgumentCount = sizeof...(ARGS);
      using Arguments = Types<ARGS...>;
   };
}

namespace Langulus::CT
{
   /// Tests whether T is a templated type                                    
   template<class T>
   concept Template = Inner::IsTemplate<T>::Value;

   /// Get the type list with all template arguments                          
   template<class T>
   using TemplateArgumentsOf = typename Inner::IsTemplate<T>::Arguments;
}
