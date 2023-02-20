///                                                                           
/// Langulus::Anyness                                                         
/// Copyright(C) 2012 Dimo Markov <langulusteam@gmail.com>                    
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "blocks/BlockSet.hpp"

namespace Langulus::Anyness
{

   ///                                                                        
   ///   Type-erased unordered set                                            
   ///                                                                        
   class UnorderedSet : public BlockSet {
   public:
      static constexpr bool Ordered = false;

      using BlockSet::BlockSet;
      using BlockSet::operator =;
   };

} // namespace Langulus::Anyness

#include "UnorderedSet.inl"
