///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "Own.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   ///   A shared pointer                                                     
   ///                                                                        
   ///   Provides ownership for a single pointer. Also, for single-element    
   /// containment, it is a lot more efficient than TMany. So, essentially    
   /// it's equivalent to std::shared_ptr                                     
   ///                                                                        
   template<class T>
   class Ref : public Own<T*> {
   protected:
      using Base = Own<T*>;
      using Self = Ref<T>;
      using Type = TypeOf<Base>;

      using Base::mValue;
      const Allocation* mEntry {};
   
      void ResetInner();

   public:
      ///                                                                     
      ///   Construction                                                      
      ///                                                                     
      constexpr Ref() noexcept;
      explicit constexpr Ref(const Ref&);
      explicit constexpr Ref(Ref&&);

      template<template<class> class S>
      requires CT::IntentMakable<S, T*>
      explicit constexpr Ref(S<Ref>&&);

      template<class A> requires CT::MakableFrom<T*, A>
      constexpr Ref(A&&);

      constexpr ~Ref();

      template<class...A> requires ::std::constructible_from<T, A...>
      auto New(A&&...) -> Ref&;

      ///                                                                     
      ///   Assignment                                                        
      ///                                                                     
      constexpr auto operator = (const Ref&) -> Ref&;
      constexpr auto operator = (Ref&&) -> Ref&;

      template<template<class> class S> requires CT::IntentAssignable<S, T*>
      auto operator = (S<Ref>&&) -> Ref&;

      template<CT::NotOwned A> requires CT::AssignableFrom<T*, A>
      auto operator = (A&&) -> Ref&;

      ///                                                                     
      ///   Capsulation                                                       
      ///                                                                     
      NOD() auto GetHandle() const -> Handle<T* const>;
      NOD() auto GetHandle()       -> Handle<T*>;
      NOD() constexpr auto GetAllocation() const noexcept -> const Allocation*;
      NOD() constexpr auto GetUses() const noexcept -> Count;
      
      using Base::operator bool;
      using Base::operator ->;
      using Base::operator *;

      /// Makes Ref CT::Resolvable                                            
      NOD() auto GetBlock() const -> Block<T*>;

      ///                                                                     
      ///   Services                                                          
      ///                                                                     
      void Reset();

      NOD() operator Ref<const T>() const noexcept requires CT::Mutable<T>;
   };


   /// Deduction guides                                                       
   template<CT::Sparse T>
   Ref(T&&) -> Ref<Deptr<T>>;

} // namespace Langulus::Anyness