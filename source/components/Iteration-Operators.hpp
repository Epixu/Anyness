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
   /// Adds prefix and suffix operators for increment and decrement.          
   /// These operators are fundamentally unsafe so the API is protected,      
   /// used mainly by handles.                                                
   ///   @tparam ID heap/stack we're iterating                                
   template<Cid ID>
   struct IterationOperators {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

      /// Offset first element to the right by the desired amount             
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return a shallow modified copy of this container                 
      template<CT::Container C>
      constexpr C operator + (this C const& self, size_t offset) noexcept {
         // Increment the heap pointer                                  
         C copy = self;
         auto& data = copy.template AccessStackById<ID>();
         if constexpr (CT::TypeErased<C>)
            data  = static_cast<uint8_t*>(data) + copy.GetStride() * offset;
         else
            data += offset;

         // Increment deep ownership entries, but only if on the stack  
         if_available(copy.template GetEntriesInner<ID>() += offset);
         return copy;
      }

      /// Offset first element to the right by the desired amount             
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator += (this C& self, size_t offset) noexcept {
         // Increment the heap pointer                                  
         auto& data = self.template AccessStackById<ID>();
         if constexpr (CT::TypeErased<C>)
            data  = static_cast<uint8_t*>(data) + self.GetStride() * offset;
         else
            data += offset;
         
         // Increment deep ownership entries, but only if on the stack  
         if_available(self.template GetEntriesInner<ID>() += offset);
         return self;
      }

      /// Prefix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator ++ (this C& self) noexcept {
         // Increment the heap pointer                                  
         auto& data = self.template AccessStackById<ID>();
         if constexpr (CT::TypeErased<C>)
            data = static_cast<uint8_t*>(data) + self.GetStride();
         else
            ++data;

         // Increment deep ownership entries, but only if on the stack  
         if_available(++self.template GetEntriesInner<ID>());
         return self;
      }

      /// Suffix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      template<CT::Container C>
      constexpr C operator ++ (this C& self, int) noexcept {
         C backup = self;
         ++self;
         return backup;
      }
      
      /// Get the element difference between two iterators                    
      ///   @attention very usafe - assumes rhs's type is same as self        
      ///   @param rhs the other iterator                                     
      ///   @return the difference                                            
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
      constexpr C operator - (this C const& self, size_t offset) noexcept {
         // Increment the heap pointer                                  
         C copy = self;
         auto& data = copy.template AccessStackById<ID>();
         if constexpr (CT::TypeErased<C>)
            data = static_cast<uint8_t*>(data) - copy.GetStride() * offset;
         else
            data -= offset;
         
         // Increment deep ownership entries, but only if on the stack  
         if_available(copy.template GetEntriesInner<ID>() -= offset);
         return copy;
      }

      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset the number of elements to offset                    
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator -= (this C& self, size_t offset) noexcept {
         // Increment the heap pointer                                  
         auto& data = self.template AccessStackById<ID>();
         if constexpr (CT::TypeErased<C>)
            data = static_cast<uint8_t*>(data) - self.GetStride() * offset;
         else
            data -= offset;
         
         // Increment deep ownership entries, but only if on the stack  
         if_available(self.template GetEntriesInner<ID>() -= offset);
         return self;
      }

      /// Prefix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator -- (this C& self) noexcept {
         // Decrement the heap pointer                                  
         auto& data = self.template AccessStackById<ID>();
         if constexpr (CT::TypeErased<C>)
            data = static_cast<uint8_t*>(data) - self.GetStride();
         else
            --data;
         
         // Decrement deep ownership entries, but only if on the stack  
         if_available(--self.template GetEntriesInner<ID>());
         return self;
      }

      /// Suffix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return a copy of the state, before modifying it                  
      template<CT::Container C>
      constexpr C operator -- (this C& self, int) noexcept {
         C backup = self;
         --self;
         return backup;
      }
   };
}
