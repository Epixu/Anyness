///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "blocks/BlockSet.hpp"


namespace Langulus::Anyness
{

   ///                                                                        
   ///   Type-erased set                                                      
   ///                                                                        
   template<bool ORDERED = false>
   struct Set : BlockSet {
      static constexpr bool Ownership = true;
      static constexpr bool Ordered = ORDERED;

      ///                                                                     
      ///   Construction                                                      
      ///                                                                     
      constexpr Set() noexcept = default;
      Set(const Set&);
      Set(Set&&);

      template<class T1, class...TAIL>
      requires CT::Inner::UnfoldInsertable<T1, TAIL...>
      Set(T1&&, TAIL&&...);

      ~Set();

      ///                                                                     
      ///   Assignment                                                        
      ///                                                                     
      Set& operator = (const Set&);
      Set& operator = (Set&&);
      Set& operator = (CT::Inner::UnfoldInsertable auto&&);

      ///                                                                     
      ///   Insertion                                                         
      ///                                                                     
      Set& operator <<  (CT::Inner::UnfoldInsertable auto&&);
      Set& operator >>  (CT::Inner::UnfoldInsertable auto&&);

      ///                                                                     
      ///   Iteration                                                         
      ///                                                                     
      NOD() Iterator begin() noexcept;
      NOD() Iterator end() noexcept;
      NOD() Iterator last() noexcept;
      NOD() ConstIterator begin() const noexcept;
      NOD() ConstIterator end() const noexcept;
      NOD() ConstIterator last() const noexcept;
   };

} // namespace Langulus::Anyness
