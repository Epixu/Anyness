///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "../BlockSet.hpp"


namespace Langulus::Anyness
{

   /// Get iterator to first element                                          
   ///   @return an iterator to the first element, or end if empty            
   LANGULUS(INLINED)
   typename BlockSet::Iterator BlockSet::begin() noexcept {
      if (IsEmpty())
         return end();

      // Seek first valid info, or hit sentinel at the end              
      auto info = GetInfo();
      while (not *info) ++info;

      const auto offset = info - GetInfo();
      return {
         info, GetInfoEnd(),
         GetInner(offset)
      };
   }

   /// Get iterator to end                                                    
   ///   @return an iterator to the end element                               
   LANGULUS(INLINED)
   typename BlockSet::Iterator BlockSet::end() noexcept {
      return {GetInfoEnd(), GetInfoEnd(), {}};
   }

   /// Get iterator to the last element                                       
   ///   @return an iterator to the last element, or end if empty             
   LANGULUS(INLINED)
   typename BlockSet::Iterator BlockSet::last() noexcept {
      if (IsEmpty())
         return end();

      // Seek first valid info in reverse, until one past first is met  
      auto info = GetInfoEnd();
      while (info >= GetInfo() and not *--info);

      const auto offset = info - GetInfo();
      return {
         info, GetInfoEnd(),
         GetInner(offset)
      };
   }

   /// Get iterator to first element                                          
   ///   @return a constant iterator to the first element, or end if empty    
   LANGULUS(INLINED)
   typename BlockSet::ConstIterator BlockSet::begin() const noexcept {
      if (IsEmpty())
         return end();

      // Seek first valid info, or hit sentinel at the end              
      auto info = GetInfo();
      while (not *info) ++info;

      const auto offset = info - GetInfo();
      return {
         info, GetInfoEnd(), 
         GetInner(offset)
      };
   }

   /// Get iterator to end                                                    
   ///   @return a constant iterator to the end element                       
   LANGULUS(INLINED)
   typename BlockSet::ConstIterator BlockSet::end() const noexcept {
      return {GetInfoEnd(), GetInfoEnd(), {}};
   }

   /// Get iterator to the last valid element                                 
   ///   @return a constant iterator to the last element, or end if empty     
   LANGULUS(INLINED)
   typename BlockSet::ConstIterator BlockSet::last() const noexcept {
      if (IsEmpty())
         return end();

      // Seek first valid info in reverse, until one past first is met  
      auto info = GetInfoEnd();
      while (info >= GetInfo() and not *--info);

      const auto offset = info - GetInfo();
      return {
         info, GetInfoEnd(),
         GetInner(offset)
      };
   }

   /// Execute a call for each key inside the set                             
   ///   @tparam REVERSE - whether or not to iterate in reverse               
   ///   @tparam F - the call to execute for each key (deducible)             
   ///   @param call - the function to execute for each key                   
   ///   @return the number of successfull executions                         
   template<bool REVERSE, class F>
   Count BlockSet::ForEach(F&& call) const {
      if (IsEmpty())
         return 0;

      using A = Decay<ArgumentOf<F>>;
      using R = ReturnOf<F>;

      static_assert(CT::Data<A>, "F's argument must be a data type");

      if constexpr (CT::TypedPair<A>) {
         // If the pair is statically typed, we check contained types   
         // against it prior to iterating                               
         using K = typename A::Key;
         if (not KeyIs<K>()) {
            // Key mismatch, no need to iterate at all                  
            return 0;
         }
      }

      // Prepare for the loop                                           
      constexpr bool HasBreaker = CT::Bool<R>;
      auto key = mKeys.GetElement(REVERSE ? -1 : 0);
      auto inf = REVERSE ? mInfo + GetReserved() - 1 : mInfo;
      const auto infEnd = REVERSE ? mInfo - 1 : mInfo + GetReserved();
      Count executions {};

      while (inf != infEnd) {
         if (*inf) {
            ++executions;

            // Execute function for each valid pair                     
            if constexpr (HasBreaker) {
               if constexpr (CT::TypedPair<A>) {
                  // The pair is statically typed, so we need to access 
                  // the elements by the provided types                 
                  using K = typename A::Key;
                  if (not call(key.template Get<K>())) {
                     // Early return, if function returns a false bool  
                     return executions;
                  }
               }
               else {
                  // The pair is dynamically typed, so we directly      
                  // forward the element blocks                         
                  if (not call(key)) {
                     // Early return, if function returns a false bool  
                     return executions;
                  }
               }
            }
            else {
               if constexpr (CT::TypedPair<A>) {
                  // The pair is statically typed, so we need to access 
                  // the elements by the provided types                 
                  using K = typename A::Key;
                  call(key.template Get<K>());
               }
               else {
                  // The pair is dynamically typed, so we directly      
                  // forward the element blocks                         
                  call(key);
               }
            }
         }

         // Next element                                                
         if constexpr (REVERSE) {
            --inf;
            key.Prev();
         }
         else {
            ++inf;
            key.Next();
         }
      }

      return executions;
   }

   /// Iterate and execute call for each element                              
   ///   @attention assumes set is not empty, and part is typed               
   ///   @param call - the function to execute for each element of type T     
   ///   @return the number of executions that occured                        
   template<class R, CT::Data A, bool REVERSE, bool MUTABLE, class F>
   Count BlockSet::ForEachInner(Block& part, F&& call) {
      LANGULUS_ASSUME(DevAssumes, not IsEmpty(), "Set is empty");
      if (not part.mType->CastsTo<A, true>())
         return 0;
       
      constexpr bool HasBreaker = CT::Bool<R>;
      Count done {};
      Count index {};

      while (index < mKeys.mReserved) {
         if (not mInfo[index]) {
            ++index;
            continue;
         }

         if constexpr (REVERSE) {
            if constexpr (HasBreaker) {
               if (not call(part.Get<A>(mKeys.mReserved - index - 1)))
                  return ++done;
            }
            else call(part.Get<A>(mKeys.mReserved - index - 1));
         }
         else {
            if constexpr (HasBreaker) {
               if (not call(part.Get<A>(index)))
                  return ++done;
            }
            else call(part.Get<A>(index));
         }

         ++index;
         ++done;
      }

      return done;
   }
   
   /// Iterate all keys inside the map, and perform f() on them               
   /// You can break the loop, by returning false inside f()                  
   ///   @param f - the function to call for each key block                   
   ///   @return the number of successful f() executions                      
   template<bool REVERSE, bool MUTABLE, class F>
   Count BlockSet::ForEachElement(Block& part, F&& call) {
      if (IsEmpty())
         return 0;

      using A = ArgumentOf<F>;
      using R = ReturnOf<F>;

      static_assert(CT::Block<A>,
         "Function argument must be a CT::Block type");
      static_assert(CT::Constant<A> or MUTABLE,
         "Non constant iterator for constant memory block");

      Count index {};
      while (index < GetReserved()) {
         if (not mInfo[index]) {
            ++index;
            continue;
         }

         A block = part.GetElement(index);
         if constexpr (CT::Bool<R>) {
            if (not call(block))
               return ++index;
         }
         else call(block);

         ++index;
      }

      return index;
   }

   /// Iterate and execute call for each element                              
   ///   @param call - the function to execute for each element of type T     
   ///   @return the number of executions that occured                        
   template<class R, CT::Data A, bool REVERSE, bool SKIP, bool MUTABLE, class F>
   Count BlockSet::ForEachDeepInner(Block& part, F&& call) {
      using DA = Decay<A>;

      Count counter = 0;
      if constexpr (CT::Deep<DA>) {
         using BlockType = Conditional<MUTABLE, DA*, const DA*>;

         if (part.IsDeep()) {
            // Iterate using a block type                               
            ForEachInner<void, BlockType, REVERSE, MUTABLE>(part,
               [&counter, &call](BlockType group) {
                  counter += const_cast<DA*>(group)->
                     template ForEachDeepInner<R, A, REVERSE, SKIP, MUTABLE>(
                        Forward<F>(call));
               }
            );
         }

         return counter;
      }
      else {
         if (part.IsDeep()) {
            // Iterate deep keys/values using non-block type            
            using BlockType = Conditional<MUTABLE, Block&, const Block&>;
            ForEachInner<void, BlockType, REVERSE, MUTABLE>(part,
               [&counter, &call](BlockType group) {
                  counter += const_cast<Block&>(group).
                     template ForEachDeepInner<R, A, REVERSE, SKIP, MUTABLE>(
                        Forward<F>(call));
               }
            );
         }
         else {
            // Equivalent to non-deep iteration                         
            counter += ForEachInner<R, A, REVERSE, MUTABLE>(part, Forward<F>(call));
         }

         return counter;
      }
   }

   /// Iterate all keys inside the map, and perform f() on them               
   /// You can break the loop, by returning false inside f()                  
   ///   @param f - the function to call for each element block               
   ///   @return the number of successful f() executions                      
   template<bool REVERSE, bool MUTABLE, class F>
   LANGULUS(INLINED)
   Count BlockSet::ForEachElement(F&& f) {
      return ForEachElement<REVERSE, MUTABLE>(mKeys, Forward<F>(f));
   }

   template<bool REVERSE, class F>
   LANGULUS(INLINED)
   Count BlockSet::ForEachElement(F&& f) const {
      return const_cast<BlockSet&>(*this).template
         ForEachElement<REVERSE, false>(Forward<F>(f));
   }

   /// Iterate keys inside the map, and perform a set of functions on them    
   /// depending on the contained type                                        
   /// You can break the loop, by returning false inside f()                  
   ///   @param f - the functions to call for each key block                  
   ///   @return the number of successful f() executions                      
   template<bool REVERSE, bool MUTABLE, class... F>
   LANGULUS(INLINED)
   Count BlockSet::ForEach(F&&... f) {
      if (IsEmpty())
         return 0;

      Count result = 0;
      (void) (... or (
         0 != (result = ForEachInner<ReturnOf<F>, ArgumentOf<F>, REVERSE, MUTABLE>(
            mKeys, Forward<F>(f))
         )
      ));
      return result;
   }

   template<bool REVERSE, class... F>
   LANGULUS(INLINED)
   Count BlockSet::ForEach(F&&... f) const {
      return const_cast<BlockSet&>(*this).template
         ForEach<REVERSE, false>(Forward<F>(f)...);
   }

   /// Iterate each subblock of keys inside the set, and perform a set of     
   /// functions on them                                                      
   ///   @param f - the functions to call for each key block                  
   ///   @return the number of successful f() executions                      
   template<bool REVERSE, bool SKIP, bool MUTABLE, class... F>
   LANGULUS(INLINED)
   Count BlockSet::ForEachDeep(F&&... f) {
      if (IsEmpty())
         return 0;

      Count result = 0;
      (void) (... or (
         0 != (result = ForEachDeepInner<ReturnOf<F>, ArgumentOf<F>, REVERSE, SKIP, MUTABLE>(
            mKeys, Forward<F>(f))
         )
      ));
      return result;
   }

   template<bool REVERSE, bool SKIP, class... F>
   LANGULUS(INLINED)
   Count BlockSet::ForEachDeep(F&&... f) const {
      return const_cast<BlockSet&>(*this).template
         ForEachDeep<REVERSE, SKIP, false>(Forward<F>(f)...);
   }


   ///                                                                        
   ///   Set iterator                                                         
   ///                                                                        

   /// Construct an iterator                                                  
   ///   @param info - the info pointer                                       
   ///   @param sentinel - the end of info pointers                           
   ///   @param key - pointer to the key element                              
   ///   @param value - pointer to the value element                          
   template<bool MUTABLE>
   LANGULUS(INLINED)
   BlockSet::TIterator<MUTABLE>::TIterator(
      const InfoType* info, 
      const InfoType* sentinel, 
      const Block& value
   ) noexcept
      : mInfo {info}
      , mSentinel {sentinel}
      , mKey {value} {}

   /// Prefix increment operator                                              
   ///   @attention assumes iterator points to a valid element                
   ///   @return the modified iterator                                        
   template<bool MUTABLE>
   LANGULUS(INLINED)
   typename BlockSet::TIterator<MUTABLE>& BlockSet::TIterator<MUTABLE>::operator ++ () noexcept {
      if (mInfo == mSentinel)
         return *this;

      // Seek next valid info, or hit sentinel at the end               
      const auto previous = mInfo;
      while (not *++mInfo);
      const auto offset = mInfo - previous;
      mKey.mRaw += offset * mKey.GetStride();
      return *this;
   }

   /// Suffix increment operator                                              
   ///   @attention assumes iterator points to a valid element                
   ///   @return the previous value of the iterator                           
   template<bool MUTABLE>
   LANGULUS(INLINED)
   typename BlockSet::TIterator<MUTABLE> BlockSet::TIterator<MUTABLE>::operator ++ (int) noexcept {
      const auto backup = *this;
      operator ++ ();
      return backup;
   }

   /// Compare unordered map entries                                          
   ///   @param rhs - the other iterator                                      
   ///   @return true if entries match                                        
   template<bool MUTABLE>
   LANGULUS(INLINED)
   bool BlockSet::TIterator<MUTABLE>::operator == (const TIterator& rhs) const noexcept {
      return mInfo == rhs.mInfo;
   }

   /// Iterator access operator                                               
   ///   @return a pair at the current iterator position                      
   template<bool MUTABLE>
   LANGULUS(INLINED)
   Any BlockSet::TIterator<MUTABLE>::operator * () const noexcept {
      return {Disown(mKey)};
   }

   /// Explicit bool operator, to check if iterator is valid                  
   template<bool MUTABLE>
   LANGULUS(INLINED)
   constexpr BlockSet::TIterator<MUTABLE>::operator bool() const noexcept {
      return mInfo != mSentinel;
   }

} // namespace Langulus::Anyness
