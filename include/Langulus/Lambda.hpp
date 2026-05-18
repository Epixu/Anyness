///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Utils/Types.hpp"


namespace Langulus
{
   namespace Inner
   {
      ///                                                                     
      /// These function declarations are used to inspect lambdas.            
      /// You can use it to extract the argument types of functions by using  
      /// decltype on the function return.                                    
      /// Handles functors, member/standing function pointers and lambdas.    
      ///                                                                     
      
      ///                                                                     
      template<class R, class F, class...AN>
      Types<AN...> GetFunctionArguments(R(F::*)(AN...) const) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }
      template<class R, class F, class...AN>
      Types<AN...> GetFunctionArguments(R(F::*)(AN...)) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }

      template<class R, class...AN>
      Types<AN...> GetFunctionArguments(R(*)(AN...)) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }

      template<class F>
      decltype(GetFunctionArguments(&F::operator())) GetFunctionArguments(F) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }

      ///                                                                     
      template<class R, class F, class...AN>
      R GetFunctionReturn(R(F::*)(AN...) const) {
         static_assert(false, "Calling GetFunctionReturn is ill-formed");
      }
      template<class R, class F, class...AN>
      R GetFunctionReturn(R(F::*)(AN...)) {
         static_assert(false, "Calling GetFunctionReturn is ill-formed");
      }

      template<class R, class...AN>
      R GetFunctionReturn(R(*)(AN...)) {
         static_assert(false, "Calling GetFunctionReturn is ill-formed");
      }

      template<class F>
      decltype(GetFunctionReturn(&F::operator())) GetFunctionReturn(F) {
         static_assert(false, "Calling GetFunctionReturn is ill-formed");
      }
   }

   /// Get the type of the first argument of a function                       
   ///   @attention will give void if no arguments                            
   ///   @tparam F anything invokable, like functor/member function/lambda    
   template<class F>
   using ArgumentOf = typename decltype(Inner::GetFunctionArguments(LglsFake(F)))::First;

   /// Get a type list corresponding to the function arguments                
   ///   @attention will give an empty type list if no arguments              
   ///   @tparam F anything invokable, like functor/member function/lambda    
   template<class F>
   using ArgumentsOf = decltype(Inner::GetFunctionArguments(LglsFake(F)));

   /// Get the return type of a function                                      
   ///   @tparam F anything invokable, like functor/member function/lambda    
   template<class F>
   using ReturnOf = decltype(Inner::GetFunctionReturn(LglsFake(F)));
}
