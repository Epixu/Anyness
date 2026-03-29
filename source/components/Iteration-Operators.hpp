///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Adds +, -, +=, -=, ++ prefix/suffix, -- prefix/suffix operators.       
   /// Adds - operator for difference between two containers.                 
   /// These operators are fundamentally unsafe so the API is protected.      
   /// Used mainly by handles. For maps and sets, these handles may point to  
   /// uninitialized values.                                                  
   ///   @tparam ID heap provider we're iterating                             
   template<Cid ID>
   struct IterationOperators {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

      /// Offset first element to the right by the desired amount             
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return a shallow modified copy of this container                 
      template<CT::Container C>
      constexpr C operator + (this C const& self, size_t offset) assumptious {
         C copy = self;
         return copy += offset;
      }

      /// Offset first element to the right by the desired amount             
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator += (this C& self, size_t offset) assumptious {
         auto& data = self.template AccessHeapProvider<ID>();
         LglsAssumeDevAndOptimize(data, "Invalid heap");

         if constexpr (CT::TypeErased<C>) {
            data = static_cast<uint8_t*>(data) + self.GetStride() * offset;

            if constexpr (requires { self.template GetEntriesInner<ID>(); }) {
               auto& entries = self.template GetEntriesInner<ID>();
               if (entries)
                  entries += self.GetIndirections() * offset;
            }
         }
         else {
            data += offset;

            if constexpr (CT::Sparse<TypeOf<C>>
            and requires { self.template GetEntriesInner<ID>(); }) {
               auto& entries = self.template GetEntriesInner<ID>();
               if (entries)
                  entries += IndirectsOf<TypeOf<C>> * offset;
            }
         }

         return self;
      }

      /// Prefix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator ++ (this C& self) assumptious {
         return (self += 1);
      }

      /// Suffix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      template<CT::Container C>
      constexpr C operator ++ (this C& self, int) assumptious {
         C backup = self;
         self += 1;
         return backup;
      }
      
      /// Get the element difference between two iterators                    
      ///   @attention very usafe - assumes rhs's type is same as self        
      ///   @param rhs the other iterator                                     
      ///   @return the difference in number of elements                      
      template<CT::Container C, CT::Container RHS>
      constexpr auto operator - (this C const& self, RHS const& rhs)
      noexcept(not CT::TypeErased<C> and not LANGULUS(SAFE)) -> ::std::ptrdiff_t {
         if constexpr (CT::TypeErased<C>) {
            ::std::ptrdiff_t diff = self.template GetRawAs<uint8_t>()
                                  -  rhs.template GetRawAs<uint8_t>();
            LglsAssumeDev((diff % self.GetStride()) == 0, "Unaligned difference");
            return diff / self.GetStride();
         }
         else return self.GetRaw() - rhs.GetRaw();
      }
      
      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return a shallow modified copy of this container                 
      template<CT::Container C>
      constexpr C operator - (this C const& self, size_t offset) assumptious {
         C copy = self;
         return (copy -= offset);
      }

      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator -= (this C& self, size_t offset) assumptious {
         auto& data = self.template AccessHeapProvider<ID>();
         LglsAssumeDevAndOptimize(data, "Invalid heap");

         if constexpr (CT::TypeErased<C>) {
            data = static_cast<uint8_t*>(data) - self.GetStride() * offset;

            if constexpr (requires { self.template GetEntriesInner<ID>(); }) {
               auto& entries = self.template GetEntriesInner<ID>();
               if (entries)
                  entries -= self.GetIndirections() * offset;
            }
         }
         else {
            data -= offset;

            if constexpr (CT::Sparse<TypeOf<C>>
            and requires { self.template GetEntriesInner<ID>(); }) {
               auto& entries = self.template GetEntriesInner<ID>();
               if (entries)
                  entries -= IndirectsOf<TypeOf<C>> * offset;
            }
         }

         return self;
      }

      /// Prefix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator -- (this C& self) assumptious {
         return (self -= 1);
      }

      /// Suffix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      template<CT::Container C>
      constexpr C operator -- (this C& self, int) assumptious {
         C backup = self;
         self -= 1;
         return backup;
      }
   };
}
