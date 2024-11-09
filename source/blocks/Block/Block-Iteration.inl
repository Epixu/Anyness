///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "../Block.hpp"
#include "../../Index.inl"


namespace Langulus::Anyness
{
   
   /// Iterate each element block and execute F for it                        
   ///   @tparam REVERSE - whether to iterate in reverse                      
   ///   @param call - function to execute for each element block             
   ///   @return the number of executions                                     
   template<class TYPE> template<bool REVERSE, bool MUTABLE>
   Count Block<TYPE>::ForEachElement(auto&& call) const {
      using F = Deref<decltype(call)>;
      using A = ArgumentOf<F>;
      using R = ReturnOf<F>;

      static_assert(CT::Block<A>,
         "Function argument must be a CT::Block binary-compatible type");
      static_assert(CT::Slab<A> or CT::Constant<A> or MUTABLE,
         "Non constant iterator for constant memory block");

      Count index = REVERSE ? mCount - 1 : 0;
      const auto next = [&index] {
         if constexpr (REVERSE)  --index;
         else                    ++index;
      };

      while (index < mCount) {
         A block = GetElement(index);

         if constexpr (CT::Bool<R>) {
            // If F returns bool, you can decide when to break the loop 
            // by simply returning false                                
            if (not call(block))
               return REVERSE ? mCount - index : index + 1;
            next();
         }
         else if constexpr (CT::Exact<R, LoopControl>) {
            // Do things depending on the F's return                    
            const R loop = call(block);

            switch (loop) {
            case Loop::Break:
               return REVERSE ? mCount - index : index + 1;
            case Loop::Continue:
               next();
               break;
            case Loop::Repeat:
               break;
            case Loop::Discard:
               if constexpr (MUTABLE) {
                  // Discard is allowed only if THIS is mutable         
                  const_cast<Block*>(this)->RemoveIndex(index);
                  if constexpr (REVERSE)
                     next();
               }
               else {
                  // ...otherwise it acts like a Loop::Continue         
                  next();
               }
               break;
            }
         }
         else {
            call(block);
            next();
         }
      }

      return mCount;
   }

   template<class TYPE> template<bool REVERSE>
   Count Block<TYPE>::ForEachElement(auto&& call) {
      return const_cast<const Block*>(this)->template
         ForEachElement<REVERSE, true>(Forward<decltype(call)>(call));
   }

   /// Execute functions for each element inside container                    
   /// Each function has a distinct argument type, that is tested against the 
   /// contained type. If argument is compatible with the type, the block is  
   /// iterated, and F is executed for all elements. The rest of the provided 
   /// functions are ignored, after the first function with viable argument.  
   ///   @tparam REVERSE - whether to iterate in reverse                      
   ///   @param calls - all potential functions to iterate with               
   ///   @return the number of executions                                     
   template<class TYPE>
   template<bool REVERSE, bool MUTABLE, class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEach(F&&...calls) const {
      static_assert(sizeof...(F) > 0, "No iterators in ForEach");
      if (IsEmpty())
         return 0;

      Count result = 0;
      (void) (... or (Loop::NextLoop !=
         ForEachInner<MUTABLE, REVERSE>(Forward<F>(calls), result)
      ));
      return result;
   }

   template<class TYPE> template<bool REVERSE, class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEach(F&&...calls) {
      static_assert(sizeof...(F) > 0, "No iterators in ForEach");
      return const_cast<const Block*>(this)->template
         ForEach<REVERSE, true>(Forward<F>(calls)...);
   }

   /// Execute functions in each sub-block, inclusively                       
   /// Unlike the flat variants above, this one reaches into sub-blocks.      
   /// Each function has a distinct argument type, that is tested against the 
   /// contained type. If argument is compatible with the type, the block is  
   /// iterated, and F is executed for all elements. None of the provided     
   /// functions are ignored, unless Loop::Break is called.                   
   ///   @tparam REVERSE - whether to iterate in reverse                      
   ///   @tparam SKIP - set to false, to execute F for intermediate blocks,   
   ///                  too; otherwise will execute only for non-blocks       
   ///   @param calls - all potential functions to iterate with               
   ///   @return the number of executions                                     
   template<class TYPE>
   template<bool REVERSE, bool SKIP, bool MUTABLE, class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEachDeep(F&&...calls) const {
      static_assert(sizeof...(F) > 0, "No iterators in ForEachDeep");
      Count result = 0;
      (void)(... or (Loop::Break ==
         ForEachDeepInner<MUTABLE, REVERSE, SKIP>(Forward<F>(calls), result)
      ));
      return result;
   }

   template<class TYPE>
   template<bool REVERSE, bool SKIP, class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEachDeep(F&&...calls) {
      static_assert(sizeof...(F) > 0, "No iterators in ForEachDeep");
      return const_cast<const Block*>(this)->template
         ForEachDeep<REVERSE, SKIP, true>(Forward<F>(calls)...);
   }

   /// Same as ForEachElement, but in reverse                                 
   template<class TYPE> template<bool MUTABLE, class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEachElementRev(F&&...f) const {
      static_assert(sizeof...(f) > 0, "No iterators in ForEachElementRev");
      return ForEachElement<true, MUTABLE>(Forward<F>(f)...);
   }

   template<class TYPE> template<class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEachElementRev(F&&...f) {
      static_assert(sizeof...(f) > 0, "No iterators in ForEachElementRev");
      return ForEachElement<true, true>(Forward<F>(f)...);
   }

   /// Same as ForEach, but in reverse                                        
   template<class TYPE> template<bool MUTABLE, class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEachRev(F&&...f) const {
      static_assert(sizeof...(f) > 0, "No iterators in ForEachRev");
      return ForEach<true, MUTABLE>(Forward<F>(f)...);
   }

   template<class TYPE> template<class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEachRev(F&&...f) {
      static_assert(sizeof...(f) > 0, "No iterators in ForEachRev");
      return ForEach<true, true>(Forward<F>(f)...);
   }

   /// Same as ForEachDeep, but in reverse                                    
   template<class TYPE>
   template<bool SKIP, bool MUTABLE, class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEachDeepRev(F&&...f) const {
      static_assert(sizeof...(f) > 0, "No iterators in ForEachDeepRev");
      return ForEachDeep<true, SKIP, MUTABLE>(Forward<F>(f)...);
   }

   template<class TYPE> template<bool SKIP, class...F> LANGULUS(INLINED)
   Count Block<TYPE>::ForEachDeepRev(F&&...f) {
      static_assert(sizeof...(f) > 0, "No iterators in ForEachDeepRev");
      return ForEachDeep<true, SKIP, true>(Forward<F>(f)...);
   }

   /// Iterate and execute call for each flat element, counting each          
   /// successfull execution                                                  
   ///   @attention assumes block is not empty                                
   ///   @attention assumes block is typed                                    
   ///   @tparam REVERSE - whether to iterate in reverse                      
   ///   @param f - the function to execute for each element of type A        
   ///   @return the last 'f' result                                          
   template<class T> template<bool MUTABLE, bool REVERSE> LANGULUS(INLINED)
   LoopControl Block<T>::ForEachInner(auto&& f, Count& index) const
   noexcept(NoexceptIterator<decltype(f)>) {
      using F = Deref<decltype(f)>;
      using A = ArgumentOf<F>;
      using R = ReturnOf<F>;
      UNUSED() static constexpr auto NOE = NoexceptIterator<decltype(f)>;

      if constexpr (not TypeErased) {
         // Container is not type-erased                                
         if constexpr (CT::Deep<Decay<A>, Decay<T>>
         or (not CT::Deep<Decay<A>> and CT::DerivedFrom<T, A>)
         or (CT::Same<A, T>)
         ) {
            using IT = Conditional<MUTABLE, T&, const T&>;

            return IterateInner<MUTABLE, REVERSE>(mCount,
               [&index, &f](IT element) noexcept(NOE) -> R {
                  ++index;

                  //TODO this does only one dereference if needed, but it should actually
                  // check the difference of sparseness between A and T, and dereference as
                  // many times as needed. that way we can iterate int*** for example,
                  // even if int***** is contained
                  // it can be done on compile time without any cost whatsoever
                  if constexpr (CT::Dense<A, T> or CT::Sparse<A, T>)
                     return f( element);
                  else if constexpr (CT::Dense<A>)
                     return f(*element);
                  else
                     return f(&element);
               }
            );
         }
         else return Loop::NextLoop;
      }
      else if ((CT::Deep<Decay<A>> and IsDeep())
      or   (not CT::Deep<Decay<A>> and CastsTo<A, true>())
      ) {
         // Container is type-erased                                    
         if (mType->mIsSparse) {
            // Iterate sparse container                                 
            using DA = Conditional<MUTABLE, void*&, const void* const&>;

            return IterateInner<MUTABLE, REVERSE>(mCount,
               [&index, &f](DA element) noexcept(NOE) -> R {
                  ++index;
                  if constexpr (CT::Dense<A>) {
                     if constexpr (MUTABLE)
                        return f(*static_cast<      Deref<A>*>(element));
                     else
                        return f(*static_cast<const Deref<A>*>(element));
                  }
                  else return  f( static_cast<A>(element));
               }
            );
         }
         else {
            // Iterate dense container                                  
            using DA = Conditional<MUTABLE, Decay<A>&, const Decay<A>&>;

            return IterateInner<MUTABLE, REVERSE>(mCount,
               [&index, &f](DA element) noexcept(NOE) -> R {
                  ++index;
                  if constexpr (CT::Dense<A>)
                     return f( element);
                  else
                     return f(&element);
               }
            );
         }
      }
      else return Loop::NextLoop;
   }
   
   /// Iterate and execute call for each deep element                         
   ///   @tparam REVERSE - whether to iterate in reverse                      
   ///   @tparam SKIP - whether to execute call for intermediate blocks       
   ///   @param call - the function to execute for each element of type A     
   ///   @return the number of executions that occured                        
   template<class TYPE> template<bool MUTABLE, bool REVERSE, bool SKIP>
   LoopControl Block<TYPE>::ForEachDeepInner(auto&& call, Count& counter) const {
      using F = Deref<decltype(call)>;
      using A = ArgumentOf<F>;
      using R = ReturnOf<F>;
      static_assert(CT::Dense<A>, "Iterator must be dense value/reference");

      if constexpr (TypeErased) {
         if constexpr (CT::Deep<A>) {
            if (not SKIP or (not IsDeep() and not Is<Neat>())) {
               // Always execute for intermediate/non-deep *this        
               ++counter;

               auto b = reinterpret_cast<Deref<A>*>(const_cast<Block*>(this));
               if constexpr (CT::Bool<R>) {
                  if (not call(*b))
                     return Loop::Break;
               }
               else if constexpr (CT::Exact<R, LoopControl>) {
                  // Do things depending on the F's return              
                  R loop = call(*b);

                  while (loop == Loop::Repeat)
                     loop = call(*b);

                  switch (loop) {
                  case Loop::Break:
                     return Loop::Break;
                  case Loop::Continue:
                     break;
                  case Loop::Discard:
                     if constexpr (MUTABLE) {
                        // Discard is allowed only if THIS is mutable   
                        // You can't fully discard the topmost block,   
                        // only reset it                                
                        const_cast<Block*>(this)->Reset();
                        return Loop::Discard;
                     }
                     else {
                        // ...otherwise it acts like a Loop::Continue   
                        break;
                     }
                  }
               }
               else call(*b);
            }
         }

         if (IsDeep()) {
            // Iterate subblocks                                        
            Count intermediateCounterSink = 0;
            using SubBlock = Conditional<MUTABLE, Block<>&, const Block<>&>;

            return ForEachInner<MUTABLE, REVERSE>(
               [&counter, &call](SubBlock group) {
                  return DenseCast(group).template
                     ForEachDeepInner<MUTABLE, REVERSE, SKIP>(
                        ::std::move(call), counter);
               },
               intermediateCounterSink
            );
         }
         else if (Is<Neat>()) {
            // Iterate normalized subblocks                             
            using SubNeat = Conditional<MUTABLE, Neat&, const Neat&>;

            return ForEachInner<MUTABLE, REVERSE>(
               [&call](SubNeat neat) {
                  return neat.ForEachDeep(::std::move(call));
               },
               counter
            );
         }
         else if constexpr (not CT::Deep<A>) {
            // Equivalent to non-deep iteration                         
            return ForEachInner<MUTABLE, REVERSE>(::std::move(call), counter);
         }
      }
      else {
         if constexpr (CT::Deep<A> and (not SKIP
         or (not CT::Deep<Decay<TYPE>> and not CT::Same<TYPE, Neat>))) {
            // Always execute for intermediate/non-deep *this           
            ++counter;

            auto b = reinterpret_cast<Deref<A>*>(const_cast<Block*>(this));
            if constexpr (CT::Bool<R>) {
               if (not call(*b))
                  return Loop::Break;
            }
            else if constexpr (CT::Exact<R, LoopControl>) {
               // Do things depending on the F's return                 
               R loop = call(*b);

               while (loop == Loop::Repeat)
                  loop = call(*b);

               switch (loop) {
               case Loop::Break:
                  return Loop::Break;
               case Loop::Continue:
                  break;
               case Loop::Discard:
                  if constexpr (MUTABLE) {
                     // Discard is allowed only if THIS is mutable      
                     // You can't fully discard the topmost block,      
                     // only reset it                                   
                     const_cast<Block*>(this)->Reset();
                     return Loop::Discard;
                  }
                  else {
                     // ...otherwise it acts like a Loop::Continue      
                     break;
                  }
               }
            }
            else call(*b);
         }

         if constexpr (CT::Deep<Decay<TYPE>>) {
            // Iterate subblocks                                        
            Count intermediateCounterSink = 0;
            using SubBlock = Conditional<MUTABLE, Decay<TYPE>&, const Decay<TYPE>&>;

            return ForEachInner<MUTABLE, REVERSE>(
               [&counter, &call](SubBlock group) {
                  return DenseCast(group).template
                     ForEachDeepInner<MUTABLE, REVERSE, SKIP>(
                        ::std::move(call), counter);
               },
               intermediateCounterSink
            );
         }
         else if constexpr (CT::Same<TYPE, Neat>) {
            // Iterate normalized subblocks                             
            using SubNeat = Conditional<MUTABLE, Neat&, const Neat&>;

            return ForEachInner<MUTABLE, REVERSE>(
               [&call](SubNeat neat) {
                  return neat.ForEachDeep(::std::move(call));
               },
               counter
            );
         }
         else if constexpr (not CT::Deep<A>) {
            // Equivalent to non-deep iteration                         
            return ForEachInner<MUTABLE, REVERSE>(::std::move(call), counter);
         }
      }

      return Loop::Continue;
   }

   /// Execute a function for each element inside container                   
   /// Lowest-level element iteration function (for internal use only)        
   ///   @attention assumes A is binary compatible with the contained type    
   ///   @attention assumes block is not empty                                
   ///   @attention assumes sparseness matches                                
   ///   @tparam REVERSE - direction we're iterating in                       
   ///   @param call - the constexpr noexcept function to call on each item   
   template<class TYPE> template<bool MUTABLE, bool REVERSE> LANGULUS(INLINED)
   LoopControl Block<TYPE>::IterateInner(const Count count, auto&& f) const
   noexcept(NoexceptIterator<decltype(f)>) {
      using F = Deref<decltype(f)>;
      using A = ArgumentOf<F>;
      using R = ReturnOf<F>;

      static_assert(CT::Complete<Decay<A>> or CT::Sparse<A>,
         "Can't iterate with incomplete type, use pointer instead");
      static_assert(CT::Slab<A> or CT::Constant<Deptr<A>> or MUTABLE,
         "Non-constant iterator for constant memory is not allowed");

      LANGULUS_ASSUME(DevAssumes, IsTyped(),
         "Block is not typed");
      LANGULUS_ASSUME(DevAssumes, not IsEmpty(),
         "Block is empty", " (of type `", mType, "`)");
      LANGULUS_ASSUME(DevAssumes, IsSparse() == CT::Sparse<A>,
         "Sparseness mismatch", " (`", mType, 
         "` compared against `", MetaDataOf<A>(), "`)"
      );

      if constexpr (CT::Dense<A>) {
         LANGULUS_ASSUME(DevAssumes, (CastsTo<A, true>()),
            "Incompatible iterator type", " `", MetaDataOf<A>(),
            "` (iterating block of type `", mType, "`)"
         );
      }

      // These are used as detectors for block change while iterating   
      // Should be optimized-out when !MUTABLE                          
      using DA = Deref<A>;
      const auto raw = const_cast<Block*>(this)->GetRaw<DA>();

      // Prepare for the loop                                           
      auto data = raw;
      if constexpr (REVERSE)
         data += count - 1;

      const auto next = [&data] {
         if constexpr (REVERSE)  --data;
         else                    ++data;
      };

      auto dataEnd = REVERSE ? raw - 1 : raw + count;
      while (data != dataEnd) {
         // Execute function                                            
         if constexpr (CT::Bool<R>) {
            if (not f(*data))
               return Loop::Break;
            next();
         }
         else if constexpr (CT::Exact<R, LoopControl>) {
            // Do things depending on the F's return                    
            const LoopControl loop = f(*data);

            switch (loop.mControl) {
            case LoopControl::Break:
            case LoopControl::NextLoop:
               return loop;
            case LoopControl::Continue:
               next();
               break;
            case LoopControl::Repeat:
               break;
            case LoopControl::Discard:
               if constexpr (MUTABLE) {
                  // Discard is allowed only if THIS is mutable         
                  const_cast<Block*>(this)->RemoveIndex(raw - data);
                  if constexpr (REVERSE)
                     next();
                  else
                     --dataEnd;
               }
               else {
                  // ...otherwise it acts like a Loop::Continue         
                  next();
               }
               break;
            }
         }
         else {
            f(*data);
            next();
         }
      }

      return Loop::Continue;
   }
   
   /// Get iterator to first element                                          
   ///   @return an iterator to the first element, or end if empty            
   template<class TYPE> LANGULUS(INLINED)
   constexpr auto Block<TYPE>::begin() noexcept -> Iterator {
      if (IsEmpty())
         return end();

      if constexpr (TypeErased)
         return {GetElement(), GetRawEnd()};
      else
         return {GetRaw(), GetRawEnd()};
   }

   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   constexpr auto Block<TYPE>::begin() const noexcept -> ConstIterator {
      return const_cast<Block<TYPE>*>(this)->begin();
   }

   /// Get iterator to the last element                                       
   ///   @return an iterator to the last element, or end if empty             
   template<class TYPE> LANGULUS(INLINED)
   constexpr auto Block<TYPE>::last() noexcept -> Iterator {
      if (IsEmpty())
         return end();

      if constexpr (TypeErased)
         return {GetElement(), GetRawEnd()};
      else {
         const auto ptr = GetRaw();
         return {ptr + mCount - 1, ptr + mCount};
      }
   }

   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   constexpr auto Block<TYPE>::last() const noexcept -> ConstIterator {
      return const_cast<Block<TYPE>*>(this)->last();
   }
   
   /// Prefix increment - get next element by incrementing data pointer       
   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE>& Block<TYPE>::operator ++ () IF_UNSAFE(noexcept) {
      LANGULUS_ASSUME(DevAssumes, mRaw,
         "Block is not allocated");

      if constexpr (TypeErased) {
         LANGULUS_ASSUME(DevAssumes, IsTyped(),
            "Block is not typed");
         LANGULUS_ASSUME(DevAssumes, mRaw + mType->mSize <= mEntry->GetBlockEnd(),
            "Block limit breached");
         mRaw += mType->mSize;
      }
      else {
         LANGULUS_ASSUME(DevAssumes, mRaw + sizeof(TYPE) <= mEntry->GetBlockEnd(),
            "Block limit breached");
         mRaw += sizeof(TYPE);
      }

      return *this;
   }

   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE> const& Block<TYPE>::operator ++ () const IF_UNSAFE(noexcept) {
      return const_cast<Block&>(*this).operator++();
   }

   /// Prefix decrement - get previous element by decrementing data pointer   
   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE>& Block<TYPE>::operator -- () IF_UNSAFE(noexcept) {
      LANGULUS_ASSUME(DevAssumes, mRaw,
         "Block is not allocated");

      if constexpr (TypeErased) {
         LANGULUS_ASSUME(DevAssumes, IsTyped(),
            "Block is not typed");
         LANGULUS_ASSUME(DevAssumes, mRaw - mType->mSize >= mEntry->GetBlockStart(),
            "Block limit breached");
         mRaw -= mType->mSize;
      }
      else {
         LANGULUS_ASSUME(DevAssumes, mRaw - sizeof(TYPE) >= mEntry->GetBlockStart(),
            "Block limit breached");
         mRaw -= sizeof(TYPE);
      }

      return *this;
   }

   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE> const& Block<TYPE>::operator -- () const IF_UNSAFE(noexcept) {
      return const_cast<Block&>(*this).operator--();
   }

   /// Suffic increment - get next element by incrementing data pointer       
   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE> Block<TYPE>::operator ++ (int) const IF_UNSAFE(noexcept) {
      auto copy {*this};
      return ++copy;
   }

   /// Suffic decrement - get previous element by decrementing data pointer   
   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE> Block<TYPE>::operator -- (int) const IF_UNSAFE(noexcept) {
      auto copy {*this};
      return --copy;
   }
   
   /// Offset the handle                                                      
   ///   @param offset - the offset to apply                                  
   ///   @return the offsetted handle                                         
   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE> Block<TYPE>::operator + (Offset offset) const IF_UNSAFE(noexcept) {
      auto copy {*this};
      return copy += offset;
   }

   /// Offset the handle                                                      
   ///   @param offset - the offset to apply                                  
   ///   @return the offsetted handle                                         
   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE> Block<TYPE>::operator - (Offset offset) const IF_UNSAFE(noexcept) {
      auto copy {*this};
      return copy -= offset;
   }
   
   /// Prefix increment operator                                              
   ///   @return the next handle                                              
   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE>& Block<TYPE>::operator += (Offset offset) IF_UNSAFE(noexcept) {
      LANGULUS_ASSUME(DevAssumes, mRaw,
         "Block is not allocated");

      if constexpr (TypeErased) {
         LANGULUS_ASSUME(DevAssumes, IsTyped(),
            "Block is not typed");
         LANGULUS_ASSUME(DevAssumes, mRaw + offset * mType->mSize <= mEntry->GetBlockEnd(),
            "Block limit breached");
         mRaw += offset * mType->mSize;
      }
      else {
         LANGULUS_ASSUME(DevAssumes, mRaw + offset * sizeof(TYPE) <= mEntry->GetBlockEnd(),
            "Block limit breached");
         mRaw += offset * sizeof(TYPE);
      }

      return *this;
   }

   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE> const& Block<TYPE>::operator += (Offset offset) const IF_UNSAFE(noexcept) {
      return const_cast<Block&>(*this).operator+=(offset);
   }

   /// Prefix decrement operator                                              
   ///   @return the next handle                                              
   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE>& Block<TYPE>::operator -= (Offset offset) IF_UNSAFE(noexcept) {
      LANGULUS_ASSUME(DevAssumes, mRaw,
         "Block is not allocated");

      if constexpr (TypeErased) {
         LANGULUS_ASSUME(DevAssumes, IsTyped(),
            "Block is not typed");
         LANGULUS_ASSUME(DevAssumes, mRaw - offset * mType->mSize >= mEntry->GetBlockStart(),
            "Block limit breached");
         mRaw -= offset * mType->mSize;
      }
      else {
         LANGULUS_ASSUME(DevAssumes, mRaw - offset * sizeof(TYPE) >= mEntry->GetBlockStart(),
            "Block limit breached");
         mRaw -= offset * sizeof(TYPE);
      }

      return *this;
   }

   template<class TYPE> LANGULUS(ALWAYS_INLINED)
   Block<TYPE> const& Block<TYPE>::operator -= (Offset offset) const IF_UNSAFE(noexcept) {
      return const_cast<Block&>(*this).operator-=(offset);
   }



   /// Construct an iterator                                                  
   ///   @param start - the current iterator position                         
   ///   @param end - the ending marker                                       
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr TBlockIterator<T>::TBlockIterator(const TypeInner& start, Type const* end) noexcept
      : mValue {start}
      , mEnd   {end} {}

   /// Construct an end iterator                                              
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr TBlockIterator<T>::TBlockIterator(A::IteratorEnd) noexcept
      : mValue {nullptr}
      , mEnd   {nullptr} {}

   /// Compare two iterators                                                  
   ///   @param rhs - the other iterator                                      
   ///   @return true if iterators point to the same element                  
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr bool TBlockIterator<T>::operator == (const TBlockIterator& rhs) const noexcept {
      if constexpr (T::TypeErased)
         return mValue.mRaw == rhs.mValue.mRaw;
      else
         return mValue == rhs.mValue;
   }

   /// Compare iterator with an end marker                                    
   ///   @return true if element is at or beyond the end marker               
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr bool TBlockIterator<T>::operator == (A::IteratorEnd) const noexcept {
      if constexpr (T::TypeErased)
         return mValue.mRaw >= mEnd;
      else
         return mValue >= mEnd;
   }

   /// Prefix increment operator                                              
   ///   @attention assumes iterator points to a valid element                
   ///   @return the modified iterator                                        
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr TBlockIterator<T>& TBlockIterator<T>::operator ++ () noexcept {
      ++mValue;
      return *this;
   }

   /// Suffix increment operator                                              
   ///   @attention assumes iterator points to a valid element                
   ///   @return the previous value of the iterator                           
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr TBlockIterator<T> TBlockIterator<T>::operator ++ (int) noexcept {
      const auto backup = *this;
      operator ++ ();
      return backup;
   }

   /// Check if iterator is valid                                             
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr TBlockIterator<T>::operator bool() const noexcept {
      return *this != A::IteratorEnd {};
   }

} // namespace Langulus::Anyness