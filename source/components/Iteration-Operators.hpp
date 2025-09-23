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
   /// used mainly internally in other components and/or iterators.           
   ///   @tparam ID - heap/stack we're iterating                              
   template<unsigned ID>
   struct IterationOperators {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

      /// Offset first element to the right by the desired amount             
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset - the number of elements to offset                  
      ///   @return a shallow modified copy of this container                 
      template<CT::Container C>
      constexpr C operator + (this C const& self, size_t offset) noexcept {
         // Increment the heap pointer                                  
         C copy = self;
         auto& data = copy.template AccessStackById<ID>();
         data = static_cast<uint8_t*>(data) + copy.GetStride() * offset;

         // Increment deep ownership entries, but only if on the stack  
         if constexpr (C::template HasComponent<DeepOwnershipStack<ID>>)
            copy.DeepOwnershipStack<ID>::mEntries += offset;
         return copy;
      }

      /// Offset first element to the right by the desired amount             
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset - the number of elements to offset                  
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator += (this C& self, size_t offset) noexcept {
         // Increment the heap pointer                                  
         auto& data = self.template AccessStackById<ID>();
         data = static_cast<uint8_t*>(data) + self.GetStride() * offset;
         
         // Increment deep ownership entries, but only if on the stack  
         if constexpr (C::template HasComponent<DeepOwnershipStack<ID>>)
            self.DeepOwnershipStack<ID>::mEntries += offset;
         return self;
      }

      /// Prefix increment operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator ++ (this C& self) noexcept {
         // Increment the heap pointer                                  
         auto& data = self.template AccessStackById<ID>();
         data = static_cast<uint8_t*>(data) + self.GetStride();

         // Increment deep ownership entries, but only if on the stack  
         if constexpr (C::template HasComponent<DeepOwnershipStack<ID>>)
            ++self.DeepOwnershipStack<ID>::mEntries;
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
      
      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset - the number of elements to offset                  
      ///   @return a shallow modified copy of this container                 
      template<CT::Container C>
      constexpr C operator - (this C const& self, size_t offset) noexcept {
         // Increment the heap pointer                                  
         C copy = self;
         auto& data = copy.template AccessStackById<ID>();
         data = static_cast<uint8_t*>(data) - copy.GetStride() * offset;
         
         // Increment deep ownership entries, but only if on the stack  
         if constexpr (C::template HasComponent<DeepOwnershipStack<ID>>)
            copy.DeepOwnershipStack<ID>::mEntries -= offset;
         return copy;
      }

      /// Offset first element to the left by the desired amount              
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @param offset - the number of elements to offset                  
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator -= (this C& self, size_t offset) noexcept {
         // Increment the heap pointer                                  
         auto& data = self.template AccessStackById<ID>();
         data = static_cast<uint8_t*>(data) - self.GetStride() * offset;
         
         // Increment deep ownership entries, but only if on the stack  
         if constexpr (C::template HasComponent<DeepOwnershipStack<ID>>)
            self.DeepOwnershipStack<ID>::mEntries -= offset;
         return self;
      }

      /// Prefix decrement operator                                           
      ///   @attention this doesn't check any boundaries, use carefully       
      ///   @return reference to this, after being modified                   
      template<CT::Container C>
      constexpr C& operator -- (this C& self) noexcept {
         // Decrement the heap pointer                                  
         auto& data = self.template AccessStackById<ID>();
         data = static_cast<uint8_t*>(data) - self.GetStride();
         
         // Decrement deep ownership entries, but only if on the stack  
         if constexpr (C::template HasComponent<DeepOwnershipStack<ID>>)
            --self.DeepOwnershipStack<ID>::mEntries;
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
