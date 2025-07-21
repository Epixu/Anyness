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
      // ReSharper disable once CppFunctionDoesntReturnValue            
      GetFunctionArguments(R(F::*)(AN...) const) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }
      template<class R, class F, class...AN>
      Tif<(sizeof...(AN) > 0), Types<AN...>, Types<void>>
      // ReSharper disable once CppFunctionDoesntReturnValue            
      GetFunctionArguments(R(F::*)(AN...)) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }

      template<class R, class...AN>
      Tif<(sizeof...(AN) > 0), Types<AN...>, Types<void>>
      // ReSharper disable once CppFunctionDoesntReturnValue            
      GetFunctionArguments(R(*)(AN...)) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }

      template<class F>
      // ReSharper disable once CppFunctionDoesntReturnValue            
      decltype(GetFunctionArguments(&F::operator())) GetFunctionArguments(F) {
         static_assert(false, "Calling GetFunctionArguments is ill-formed");
      }

      ///                                                                     
      template<class R, class F, class...AN>
      // ReSharper disable once CppFunctionDoesntReturnValue            
      R GetFunctionReturn(R(F::*)(AN...) const) {
         static_assert(false, "Calling GetFunctionReturn is ill-formed");
      }
      template<class R, class F, class...AN>
      // ReSharper disable once CppFunctionDoesntReturnValue            
      R GetFunctionReturn(R(F::*)(AN...)) {
         static_assert(false, "Calling GetFunctionReturn is ill-formed");
      }

      template<class R, class...AN>
      // ReSharper disable once CppFunctionDoesntReturnValue            
      R GetFunctionReturn(R(*)(AN...)) {
         static_assert(false, "Calling GetFunctionReturn is ill-formed");
      }

      template<class F>
      // ReSharper disable once CppFunctionDoesntReturnValue            
      decltype(GetFunctionReturn(&F::operator())) GetFunctionReturn(F) {
         static_assert(false, "Calling GetFunctionReturn is ill-formed");
      }

      ///                                                                     
      template<class R, class F, class...AN>
      No IsNoexcept(R(F::*)(AN...) const) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class F, class...AN>
      Yes<> IsNoexcept(R(F::*)(AN...) const noexcept) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class F, class...AN>
      No IsNoexcept(R(F::*)(AN...)) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class F, class...AN>
      Yes<> IsNoexcept(R(F::*)(AN...) noexcept) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class...AN>
      No IsNoexcept(R(*)(AN...)) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class R, class...AN>
      Yes<> IsNoexcept(R(*)(AN...) noexcept) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

      template<class F>
      auto IsNoexcept(F) -> decltype(IsNoexcept(&F::operator())) {
         static_assert(false, "Calling IsNoexcept is ill-formed");
         return {};
      }

   } // namespace Langulus::Inner

   /// Get the type of the first argument of a function                       
   ///   @attention will give void if no arguments                            
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ArgumentOf = typename decltype(Inner::GetFunctionArguments(Fake<F>()))::First;

   /// Get a type list corresponding to the function arguments                
   ///   @attention will give an empty type list if no arguments              
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ArgumentsOf = decltype(Inner::GetFunctionArguments(Fake<F>()));

   /// Get the return type of a function                                      
   ///   @tparam F - anything invokable, like functor/member function/lambda  
   template<class F>
   using ReturnOf = decltype(Inner::GetFunctionReturn(Fake<F>()));

   /// Check if a function is noexcept                                        
   template<class F>
   static constexpr bool IsNoexcept = decltype(Inner::IsNoexcept(Fake<F>()))::Enabled;

} // namespace Langulus
