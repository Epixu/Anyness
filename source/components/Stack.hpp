#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Adds a variable to a container                                         
   /// Increases the container's bytesize                                     
   ///   @tparam T - type of the variable                                     
   ///   @tparam ID - multiple variables are supported                        
   ///   @attention same IDs serve to identify heap components as well, so    
   ///      make sure they don't overlap                                      
   ///                                                                        
   template<CT::NotVoid T, unsigned ID = 0>
   struct Stack {
      using CTTI_Component = Yes;

   protected:
      T mStack;

   public:
      constexpr Stack() = default;
      explicit constexpr Stack(Stack const&) = default;
      explicit constexpr Stack(Stack&&) = default;

      /// Intent constructor                                                  
      template<template<class> class I> requires CT::Intent<I<Stack>>
      constexpr Stack(I<Stack>&& other)
         : mStack {other.Nest(other->mStack)} {}

      constexpr Stack& operator = (Stack const&) noexcept = default;
      constexpr Stack& operator = (Stack&&) noexcept = default;

      /// Intent assignment                                                   
      template<template<class> class I> requires CT::Intent<I<Stack>>
      constexpr Stack& operator = (I<Stack>&& other) {
         mStack = other.Nest(other->mStack);
         return *this;
      }
   };

} // namespace Langulus::Anyness::Component
