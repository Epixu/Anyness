///                                                                           
/// Langulus::Anyness                                                         
/// Copyright(C) 2012 Dimo Markov <langulusteam@gmail.com>                    
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "TEdit.hpp"

#define TEMPLATE() template<DenseTypedBlock T>

namespace Langulus::Anyness
{

   /// Create an editor interface                                             
   ///   @param container - what container are we editing?                    
   ///   @param start - the starting marker of the selection (optional)       
   ///   @param end - the ending marker of the selection (optional)           
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   TEdit<T>::TEdit(T& container, Offset start, Offset end) noexcept
      : mSource(container)
      , mStart(start)
      , mEnd(end) {}

   /// Get the container we're editing                                        
   ///   @return a constant reference to the source container                 
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   const T& TEdit<T>::GetSource() const noexcept {
      return mSource;
   }

   /// Get the start of the selection                                         
   ///   @return the start of the selection                                   
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   Offset TEdit<T>::GetStart() const noexcept {
      return mStart;
   }

   /// Get the end of the selection                                           
   ///   @return the end of the selection                                     
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   Offset TEdit<T>::GetEnd() const noexcept {
      return mEnd;
   }

   /// Get the size of the selection                                          
   ///   @return the size of the selection                                    
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   Count TEdit<T>::GetLength() const noexcept {
      return mEnd - mStart;
   }

   /// Access an element at a given index, relative to the selection (const)  
   ///   @param index - the index to get                                      
   ///   @return a reference to the element at that index                     
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   auto& TEdit<T>::operator[] (Offset index) const noexcept {
      return mSource[mStart + index];
   }

   /// Access an element at a given index, relative to the selection          
   ///   @param index - the index to get                                      
   ///   @return a reference to the element at that index                     
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   auto& TEdit<T>::operator[] (Offset index) noexcept {
      return mSource[mStart + index];
   }

   /// Concatenate at the end of the selection                                
   ///   @param other - the container to concatenate                          
   ///   @return a reference to the editor for chaining                       
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   TEdit<T>& TEdit<T>::operator << (const T& other) {
      mSource.InsertBlockAt(other, mEnd);
      return *this;
   }

   /// Concatenate at the start of the selection                              
   ///   @param other - the container to concatenate                          
   ///   @return a reference to the editor for chaining                       
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   TEdit<T>& TEdit<T>::operator >> (const T& other) {
      const auto concatenated = mSource.InsertBlockAt(other, mStart);
      mStart += concatenated;
      mEnd += concatenated;
      return *this;
   }

   /// Replace the selection with an element sequence                         
   /// After replacement, the selection will collapse to the end of the       
   /// replacement                                                            
   ///   @param other - the container to use for replacement                  
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   TEdit<T>& TEdit<T>::Replace(const T& other) {
      if constexpr (CT::POD<CTTI_InnerType> || CT::Sparse<CTTI_InnerType>) {
         const auto offset = mStart * mSource.GetStride();

         // Optimization for POD/sparse containers that spares the      
         // execution of move constructors of all elements              
         if (other.GetCount() > GetLength()) {
            // Replacement is bigger, we need a bit more space          
            const auto surplus = (other.GetCount() + mStart) - mEnd;
            mSource.AllocateMore<false, true>(mSource.GetCount() + surplus);

            MoveMemory(
               mSource.GetRaw() + offset + surplus,
               mSource.GetRaw() + offset,
               mSource.GetCount() - offset - surplus
            );
         }
         else if (other.GetCount() < GetLength()) {
            // Replacement is smaller, move data backwards              
            const auto excess = GetLength() - other.GetCount();
            mSource.RemoveIndexAt(mStart + other.GetCount(), excess);
         }

         // Copy new data over the old one                              
         CopyMemory(
            mSource.GetRaw() + offset,
            other.GetRaw(), 
            other.GetByteSize()
         );
      }
      else {
         // Generalized, but safe replacement                           
         if (GetLength()) {
            //TODO can be optimized further, by avoiding the redundant move
            mSource.RemoveIndex(mStart, GetLength());
            mSource.InsertBlockAt(other, mStart);
         }
      }

      // Move marker and collapse selection                             
      mStart += other.GetCount();
      mEnd = mStart;
      return *this;
   }

   /// Insert single element at the end of the selection                      
   ///   @param other - the element to insert                                 
   ///   @return a reference to the editor for chaining                       
   TEMPLATE()
   LANGULUS(ALWAYSINLINE)
   TEdit<T>& TEdit<T>::operator << (const CTTI_InnerType& other) {
      mSource.InsertAt(other, mEnd);
      return *this;
   }

   /// Insert single element at the start of the selection                    
   ///   @param other - the element to insert                                 
   ///   @return a reference to the editor for chaining                       
   TEMPLATE()
   LANGULUS(ALWAYSINLINE)
   TEdit<T>& TEdit<T>::operator >> (const CTTI_InnerType& other) {
      const auto concatenated = mSource.InsertAt(other, mStart);
      mStart += concatenated;
      mEnd += concatenated;
      return *this;
   }

   /// Replace the selection with a single element                            
   /// After replacement, the selection will collapse to the end of the       
   /// replacement                                                            
   ///   @param other - the element to use for replacement                    
   ///   @return a reference to the editor for chaining                       
   TEMPLATE()
   LANGULUS(ALWAYSINLINE)
   TEdit<T>& TEdit<T>::Replace(const CTTI_InnerType& other) {
      return Replace(T::Wrap(other));
   }

   /// Delete selection (collapsing it), or delete symbol after collapsed     
   /// selection marker                                                       
   ///   @return a reference to the editor for chaining                       
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   TEdit<T>& TEdit<T>::Delete() {
      const auto length = GetLength();
      if (length) {
         mSource.RemoveIndex(mStart, length);
         mEnd = mStart;
      }
      else if (mStart < mSource.GetCount()) {
         mSource.RemoveIndex(mStart, 1);
      }
      
      if (mSource.IsEmpty())
         mStart = mEnd = 0;
      else if (mStart >= mSource.GetCount())
         mStart = mEnd = mSource.GetCount() - 1;
      return *this;
   }

   /// Delete selection (collapsing it), or delete symbol before a collapsed  
   /// selection marker                                                       
   ///   @return a reference to the editor for chaining                       
   TEMPLATE() LANGULUS(ALWAYSINLINE)
   TEdit<T>& TEdit<T>::Backspace() {
      const auto length = GetLength();
      if (length) {
         mSource.RemoveIndex(mStart, length);
         mEnd = mStart;
      }
      else if (mStart > 0 && !mSource.IsEmpty()) {
         mSource.RemoveIndex(mStart - 1, 1);
         mEnd = --mStart;
      }

      if (mSource.IsEmpty())
         mStart = mEnd = 0;
      else if (mStart >= mSource.GetCount())
         mStart = mEnd = mSource.GetCount() - 1;
      return *this;
   }

} // namespace Langulus::Anyness

#undef TEMPLATE