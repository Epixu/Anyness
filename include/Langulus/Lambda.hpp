///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Types.hpp"


namespace Langulus
{
   
   namespace Inner
   {

      ///                                                                     
      /// These function declarations are used to decompose lambdas           
      /// You can use it to extract the argument types of functions, by using 
      /// decltype on the function return.                                    
      /// Handles functors, member/standing function pointers, lambdas.       
      ///                                                                     
      
      ///                                                                     
      template<class R, class F, class...AN>
      Tif<(sizeof...(AN) > 0), Types<AN...>, Types<void>>
      GetFunctionArguments(R(F::*)(AN...) const) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }
      template<class R, class F, class...AN>
      Tif<(sizeof...(AN) > 0), Types<AN...>, Types<void>>
      GetFunctionArguments(R(F::*)(AN...)) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }

      template<class R, class...AN>
      Tif<(sizeof...(AN) > 0), Types<AN...>, Types<void>>
      GetFunctionArguments(R(*)(AN...)) {
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

      ///                                                                     
      template<class R, class F, class...AN>
      void IsNoexcept(R(F::*f)(AN...) const) noexcept(noexcept((Fake<F>().*f)(Fake<AN>()...))) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
      }

      template<class R, class F, class...AN>
      void IsNoexcept(R(F::*f)(AN...)) noexcept(noexcept((Fake<F>().*f)(Fake<AN>()...))) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
      }

      template<class R, class...AN>
      void IsNoexcept(R(*f)(AN...)) noexcept(noexcept(f(Fake<AN>()...))) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
      }

      template<class F>
      void IsNoexcept(F) noexcept(noexcept(IsNoexcept(&F::operator()))) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
      }

   } // namespace Langulus::Inner

   /// Get the type of the first argument of a function                       
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ArgumentOf = typename decltype(Inner::GetFunctionArguments(Fake<F>()))::First;

   /// Get a type list corresponding to the function arguments                
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ArgumentsOf = decltype(Inner::GetFunctionArguments(Fake<F>()));

   /// Get the return type of a function                                      
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ReturnOf = decltype(Inner::GetFunctionReturn(Fake<F>()));

   /// Check if a lambda call is noexcept                                     
   template<class F>
   static constexpr bool IsNoexcept = noexcept(Inner::IsNoexcept(Fake<F>()));

} // namespace Langulus
