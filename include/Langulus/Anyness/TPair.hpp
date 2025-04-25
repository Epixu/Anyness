///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../../../source/Container.hpp"
#include "../../../source/components/Stack.hpp"
#include "../../../source/components/Assignment.hpp"
#include "../../../source/components/Typed-Static.hpp"


namespace Langulus::CT
{

   /// Concept for recognizing arguments, with which a statically typed       
   /// pair can be constructed                                                
   template<class K, class V, class P>
   concept PairConstructible = Pair<P> and NotReference<K, V>
       and (IntentOf<P>::Shallow or (
            IntentConstructibleAlt<typename IntentOf<P>::template As<K>>
        and IntentConstructibleAlt<typename IntentOf<P>::template As<V>>));

   /// Concept for recognizing argument, with which a statically typed        
   /// pair can be assigned                                                   
   template<class K, class V, class P>
   concept PairAssignable = Pair<P> and NotReference<K, V>
       and (IntentOf<P>::Shallow or (
            IntentAssignableAlt<typename IntentOf<P>::template As<K>>
        and IntentAssignableAlt<typename IntentOf<P>::template As<V>>));

   /// Concept for recognizing argument, against which a pair can be compared 
   template<class K, class V, class P>
   concept PairComparable = Pair<P>
       and Comparable<K, typename Deint<P>::Key>
       and Comparable<V, typename Deint<P>::Value>;

} // namespace Langulus::CT

namespace Langulus::Anyness
{
   namespace Inner
   {

      template<CT::NotVoid K, CT::NotVoid V>
      using TPairBase = Container<
         Component::Stack<K, 0>,             // Key on the stack        
         Component::Stack<V, 1>,             // Value on the stack      
         Component::TypedStatic<DMeta, K, 0>,// Statically typed key    
         Component::TypedStatic<DMeta, V, 1>,// Statically typed value  
         Component::Assignment               // Allows for assignment   
      >;

   } // namespace Langulus::Anyness::Inner

   ///                                                                        
   ///   A helper structure for pairing keys and values of any type           
   ///                                                                        
   ///   This is the statically typed pair, and it can be used with           
   /// references, as well as dense or sparse values. When key or value types 
   /// are references, the TPair acts as a simple intermediate type, often    
   /// used to access elements inside maps.                                   
   ///   @attention TPair is not binary-compatible with its type-erased       
   ///      counterpart Pair                                                  
   ///                                                                        
   template<CT::NotVoid K, CT::NotVoid V>
   struct TPair : Inner::TPairBase<K, V> {
      using CTTI_Typed = Types<K, V>;
      using CTTI_Pair  = Yes;

      using Base = Inner::TPairBase<K, V>;
      using Key = K;
      using Val = V;
      using Value = V;

      constexpr TPair() = default;
      constexpr TPair(TPair const&) = default;
      constexpr TPair(TPair&&) = default;

      template<class P> requires CT::PairConstructible<K, V, P>
      constexpr TPair(P&& other)
         : Base {other.template Forward<typename Decay<Deint<P>>::Base>()} {}

      template<class ALT_K, class ALT_V>
      requires (CT::ConstructibleFrom<K, ALT_K>
           and  CT::ConstructibleFrom<V, ALT_V>
           and  CT::NotReference<K, V>)
      constexpr TPair(ALT_K&&, ALT_V&&);

      constexpr TPair(K&&, V&&) noexcept requires CT::Reference<K, V>;
   };

} // namespace Langulus::Anyness
