///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "Pair.hpp"


namespace Langulus::CT
{

   /// Concept for recognizing arguments, with which a statically typed       
   /// pair can be constructed                                                
   template<class K, class V, class A>
   concept PairMakable = Pair<Desem<A>> and not Reference<K, V>
       and (SemanticOf<A>::Shallow or (
            Inner::SemanticMakableAlt<typename SemanticOf<A>::template As<K>>
        and Inner::SemanticMakableAlt<typename SemanticOf<A>::template As<V>>));

   /// Concept for recognizing argument, with which a statically typed        
   /// pair can be assigned                                                   
   template<class K, class V, class A>
   concept PairAssignable = Pair<Desem<A>> and not Reference<K, V>
       and (SemanticOf<A>::Shallow or (
            Inner::SemanticAssignableAlt<typename SemanticOf<A>::template As<K>>
        and Inner::SemanticAssignableAlt<typename SemanticOf<A>::template As<V>>));

   /// Concept for recognizing argument, against which a pair can be compared 
   template<class K, class V, class A>
   concept PairComparable = Pair<A>
       and Inner::Comparable<K, typename A::Key>
       and Inner::Comparable<V, typename A::Value>;

} // namespace Langulus::CT

namespace Langulus::Anyness
{

   ///                                                                        
   ///   A helper structure for pairing keys and values of any type           
   ///                                                                        
   template<class K, class V>
   struct TPair : A::Pair {
      using Key = K;
      using Value = V;

      LANGULUS_ABSTRACT() false;
      LANGULUS(TYPED) TPair<K, V>;

      Key   mKey;
      Value mValue;

      ///                                                                     
      ///   Construction & Assignment                                         
      ///                                                                     
      TPair() = default;
      TPair(TPair const&) = default;
      TPair(TPair&&) = default;

      template<class P> requires CT::PairMakable<K, V, P>
      TPair(P&&);

      template<class K1, class V1>
      requires (CT::MakableFrom<K, K1> and CT::MakableFrom<V, V1>
           and not CT::Reference<K, V>)
      TPair(K1&&, V1&&);

      TPair(K&&, V&&) noexcept requires CT::Reference<K, V>;

      TPair& operator = (TPair const&) = default;
      TPair& operator = (TPair&&) = default;
      template<class P> requires CT::PairAssignable<K, V, P>
      TPair& operator = (P&&);

      ///                                                                     
      ///   Capsulation                                                       
      ///                                                                     
      NOD() Hash  GetHash() const requires CT::Hashable<K, V>;
      NOD() DMeta GetKeyType() const noexcept;
      NOD() DMeta GetValueType() const noexcept;

      ///                                                                     
      ///   Comparison                                                        
      ///                                                                     
      template<class P> requires CT::PairComparable<K, V, P>
      bool operator == (const P&) const;

      operator TPair<const Deref<K>&, const Deref<V>&>() const noexcept
      requires CT::Reference<K, V>;
   };

} // namespace Langulus::Anyness
