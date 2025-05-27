///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "THandle.hpp"
#include <Langulus/Retype.hpp>


namespace Langulus::CT
{

   /// Concept for recognizing arguments, with which a statically typed       
   /// pair can be constructed                                                
   template<class K, class V, class...P>
   concept PairConstructible = NotReference<K, V> and Pair<P...>
       and ((IntentOf<P>::Shallow or (
               IntentConstructibleAlt<Retype<IntentOf<P>, K>>
           and IntentConstructibleAlt<Retype<IntentOf<P>, V>>)
       ) and ...);

   /// Concept for recognizing argument, with which a statically typed        
   /// pair can be assigned                                                   
   template<class K, class V, class...P>
   concept PairAssignable = NotReference<K, V> and Pair<P...>
       and ((IntentOf<P>::Shallow or (
               IntentAssignableAlt<Retype<IntentOf<P>, K>>
           and IntentAssignableAlt<Retype<IntentOf<P>, V>>)
       ) and ...);

   /// Concept for recognizing argument, against which a pair can be compared 
   template<class K, class V, class...P>
   concept PairComparable = Pair<P...> and ((
           Comparable<K, typename Deint<P>::Key>
       and Comparable<V, typename Deint<P>::Val>
      ) and ...);

} // namespace Langulus::CT

namespace Langulus::Anyness
{

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
   struct TPair {
   private:
      THandle<K> mKey;
      THandle<V> mVal;

   public:
      using CTTI_Typed = Types<K, V>;
      using CTTI_Pair  = Yes;

      using Key = K;
      using Val = V;

      ///                                                                     
      ///   Construction                                                      
      constexpr TPair() noexcept = default;
      constexpr TPair(TPair const&) noexcept = default;
      constexpr TPair(TPair&&) noexcept = default;

      constexpr TPair(CT::Pair auto&& other) requires CT::PairConstructible<K, V, decltype(other)>
         : mKey {IntentOf<decltype(other)>::Nest(DeintCast(other).GetKey())}
         , mVal {IntentOf<decltype(other)>::Nest(DeintCast(other).GetVal())} {}

      template<class ALT_K, class ALT_V>
      requires (CT::NotReference<K, V>
           and  CT::ConstructibleFrom<K, ALT_K>
           and  CT::ConstructibleFrom<V, ALT_V>)
      constexpr TPair(ALT_K&&, ALT_V&&);

      constexpr TPair(K, V) noexcept requires CT::Reference<K, V>;

      ///                                                                     
      ///   Assignment                                                        
      TPair& operator = (TPair const&) noexcept = default;
      TPair& operator = (TPair&&) noexcept = default;

      template<CT::Pair P> requires CT::PairAssignable<K, V, P>
      TPair& operator = (P&&);

      ///                                                                     
      ///   Capsulation                                                       
      Hash GetHash() const;

      auto& GetKey(this auto&& self) noexcept { return self.mKey; }
      auto& GetVal(this auto&& self) noexcept { return self.mVal; }
   };

} // namespace Langulus::Anyness
