///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "blocks/BlockMap.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   ///   Type-erased ordered map                                              
   ///                                                                        
   class OrderedMap : public BlockMap {
   public:
      static constexpr bool Ownership = true;
      static constexpr bool Ordered = true;

      constexpr OrderedMap() noexcept = default;
      OrderedMap(const OrderedMap&);
      OrderedMap(OrderedMap&&);

      OrderedMap(const CT::NotSemantic auto&);
      OrderedMap(CT::NotSemantic auto&);
      OrderedMap(CT::NotSemantic auto&&);
      OrderedMap(CT::Semantic auto&&);

      template<CT::Data T1, CT::Data T2, CT::Data... TAIL>
      OrderedMap(T1&&, T2&&, TAIL&&...);

      ~OrderedMap();

      OrderedMap& operator = (const OrderedMap&);
      OrderedMap& operator = (OrderedMap&&);

      OrderedMap& operator = (const CT::NotSemantic auto&);
      OrderedMap& operator = (CT::NotSemantic auto&);
      OrderedMap& operator = (CT::NotSemantic auto&&);
      OrderedMap& operator = (CT::Semantic auto&&);

      ///                                                                     
      ///   Search                                                            
      ///                                                                     
      template<CT::NotSemantic K>
      NOD() Block At(const K&);
      template<CT::NotSemantic K>
      NOD() Block operator[] (const K&);

      ///                                                                     
      ///   Insertion                                                         
      ///                                                                     
      Count Insert(const CT::NotSemantic auto&,  const CT::NotSemantic auto&);
      Count Insert(const CT::NotSemantic auto&,        CT::NotSemantic auto&);
      Count Insert(const CT::NotSemantic auto&,        CT::NotSemantic auto&&);
      Count Insert(const CT::NotSemantic auto&,        CT::Semantic    auto&&);

      Count Insert(      CT::NotSemantic auto&,  const CT::NotSemantic auto&);
      Count Insert(      CT::NotSemantic auto&,        CT::NotSemantic auto&);
      Count Insert(      CT::NotSemantic auto&,        CT::NotSemantic auto&&);
      Count Insert(      CT::NotSemantic auto&,        CT::Semantic    auto&&);

      Count Insert(      CT::NotSemantic auto&&, const CT::NotSemantic auto&);
      Count Insert(      CT::NotSemantic auto&&,       CT::NotSemantic auto&);
      Count Insert(      CT::NotSemantic auto&&,       CT::NotSemantic auto&&);
      Count Insert(      CT::NotSemantic auto&&,       CT::Semantic    auto&&);

      Count Insert(      CT::Semantic    auto&&, const CT::NotSemantic auto&);
      Count Insert(      CT::Semantic    auto&&,       CT::NotSemantic auto&);
      Count Insert(      CT::Semantic    auto&&,       CT::NotSemantic auto&&);
      Count Insert(      CT::Semantic    auto&&,       CT::Semantic    auto&&);

      Count Insert(const CT::NotSemantic auto&);
      Count Insert(      CT::NotSemantic auto&);
      Count Insert(      CT::NotSemantic auto&&);
      Count Insert(      CT::Semantic    auto&&);

      OrderedMap& operator << (const CT::NotSemantic auto&);
      OrderedMap& operator << (      CT::NotSemantic auto&);
      OrderedMap& operator << (      CT::NotSemantic auto&&);
      OrderedMap& operator << (      CT::Semantic    auto&&);

   protected:
      Count InsertUnknown(CT::Semantic auto&&, CT::Semantic auto&&);
      Count InsertUnknown(CT::Semantic auto&&);
   };

} // namespace Langulus::Anyness
