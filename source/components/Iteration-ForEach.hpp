///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Deep.hpp>
#include <Langulus/CT/ReflectAs.hpp>
#include <Langulus/Lambda.hpp>
#include <Langulus/Tag.hpp>
#include <Langulus/Assume.hpp>
#include <Langulus/MetaOf.hpp>


namespace Langulus
{
   /// Loop controls from inside ForEach lambdas when iterating containers    
   struct LoopControl {
      enum Command : int {
         Break = 0,     // Break the loop                               
         Continue = 1,  // Continue the loop                            
         Repeat = 2,    // Repeat the current element                   
         Discard = 3,   // Remove the current element                   
         NextLoop = 4   // Skip to next function in the ForEach         
      } mControl;

      LoopControl() = delete;

      constexpr LoopControl(bool a) noexcept
         : mControl {static_cast<Command>(a)} {}
      constexpr LoopControl(Command a) noexcept
         : mControl {a} {}

      explicit constexpr operator bool() const noexcept {
         return mControl == Continue or mControl == Repeat;
      }

      constexpr bool operator == (const LoopControl&) const noexcept = default;
   };

   namespace Loop
   {
      /// Break the entire iteration as a whole                               
      constexpr LoopControl Break      = LoopControl::Break;
      /// Continue to next element or function                                
      constexpr LoopControl Continue   = LoopControl::Continue;
      /// Repeat the current element                                          
      constexpr LoopControl Repeat     = LoopControl::Repeat;
      /// Remove the current element                                          
      constexpr LoopControl Discard    = LoopControl::Discard;
      /// End this iterating function and jump immediately to the next        
      constexpr LoopControl NextLoop   = LoopControl::NextLoop;
   }

   namespace Anyness
   {
      class Neat;
   }
}

namespace Langulus::Anyness::Component
{
   ///                                                                        
   /// Implements ForEach iteration interface for containers                  
   ///   @tparam ID heap/stack we're iterating                                
   template<unsigned ID>
   struct IterationForEach {
      using CTTI_Component = Yes<>;
      static constexpr int ComponentPrecedence = 3000;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      using Deep = Tmut<C, typename Deref<C>::DeepType&, typename Deref<C>::DeepType const&>;

      /// A helper structure that shows how ForEach iteration went            
      template<CT::Container C>
      struct ForEachResult {
         // Number of iterations                                        
         Count<C> count = 0;
         // Last loop control return - useful only when you want to     
         // control an outer loop depending on the iteration result     
         LoopControl control = Loop::Continue;

         // Implicitly cast to the count member, because that's the     
         // most likely use                                             
         operator Count<C>() const noexcept { return count; }
      };

   public:
      template<CT::Container C>
      auto ForEachElement(this C&&, auto&&...) -> ForEachResult<C>;
      template<CT::Container C>
      auto ForEachElementRev(this C&&, auto&&...) -> ForEachResult<C>;

      /// Execute functions for each element inside container                 
      /// Each function has a distinct argument type, that is tested against  
      /// the contained type. If argument is compatible with the type, the    
      /// container is iterated and the function - executed for all elements. 
      /// The rest of the provided functions are ignored after the first      
      /// function with viable argument                                       
      ///   @param lambdas all potential functions to iterate with            
      ///   @return the number of executions and the control end code         
      template<CT::Container C, class...F>
      auto ForEach(this C&& self, F&&...lambdas) -> ForEachResult<C> {
         static_assert(sizeof...(F) > 0, "No functions in ForEach");
         if (self.IsEmpty())
            return {};

         LoopControl loop = Loop::Break;
         Count<C> result = 0;
         (void)(... or (Loop::NextLoop != (
            loop = self.template ForEachInner<false>(::std::forward<F>(lambdas), result)
         )));

         if (loop == Loop::Discard)
            self.Reset();
         return {result, loop};
      }

      /// Do it in reverse                                                    
      template<CT::Container C, class...F>
      auto ForEachRev(this C&& self, F&&...lambdas) -> ForEachResult<C> {
         static_assert(sizeof...(F) > 0, "No functions in ForEach");
         if (self.IsEmpty())
            return {};

         LoopControl loop = Loop::Break;
         Count<C> result = 0;
         (void)(... or (Loop::NextLoop != (
            loop = self.template ForEachInner<true>(::std::forward<F>(lambdas), result)
         )));

         if (loop == Loop::Discard)
            self.Reset();
         return {result, loop};
      }

      /// Execute functions in each sub-block, inclusively                    
      /// Unlike the flat variants above, this one reaches into sub-blocks.   
      /// Each function has a distinct argument type, that is tested against  
      /// the contained type. If argument is compatible with the type, the    
      /// block is iterated, and F is executed for all elements. None of the  
      /// provided functions are ignored, unless Loop::Break is returned at   
      /// some point                                                          
      ///   @param lambdas all potential functions to iterate with            
      ///   @return the number of executions                                  
      template<CT::Container C, class...F>
      auto ForEachDeep(this C&& self, F&&...lambdas) -> ForEachResult<C> {
         static_assert(sizeof...(F) > 0, "No functions in ForEach");
         if (self.IsEmpty())
            return {};

         LoopControl loop = Loop::Break;
         Count<C> result = 0;
         (void)(... or (Loop::Break == (
            loop = self.template ForEachDeepInner<false, true>(::std::forward<F>(lambdas), result)
         )));

         if (loop == Loop::Discard)
            self.Reset();
         return {result, loop};
      }

      /// Do it in reverse                                                    
      template<CT::Container C, class...F>
      auto ForEachDeepRev(this C&& self, F&&...lambdas) -> ForEachResult<C> {
         static_assert(sizeof...(F) > 0, "No functions in ForEach");
         if (self.IsEmpty())
            return {};

         LoopControl loop = Loop::Break;
         Count<C> result = 0;
         (void)(... or (Loop::Break == (
            loop = self.template ForEachDeepInner<true, true>(::std::forward<F>(lambdas), result)
         )));

         if (loop == Loop::Discard)
            self.Reset();
         return {result, loop};
      }

      /// Do it without skipping the intermediate containers                  
      template<CT::Container C, class...F>
      auto ForEachDeepNoskip(this C&& self, F&&...lambdas) -> ForEachResult<C> {
         static_assert(sizeof...(F) > 0, "No functions in ForEach");
         LoopControl loop = Loop::Break;
         Count<C> result = 0;
         (void)(... or (Loop::Break == (
            loop = self.template ForEachDeepInner<false, false>(::std::forward<F>(lambdas), result)
         )));

         if (loop == Loop::Discard)
            self.Reset();
         return {result, loop};
      }

      /// Do it without skipping the intermediate containers in reverse       
      template<CT::Container C, class...F>
      auto ForEachDeepNoskipRev(this C&& self, F&&...lambdas) -> ForEachResult<C> {
         static_assert(sizeof...(F) > 0, "No functions in ForEach");
         LoopControl loop = Loop::Break;
         Count<C> result = 0;
         (void)(... or (Loop::Break == (
            loop = self.template ForEachDeepInner<true, false>(::std::forward<F>(lambdas), result)
         )));

         if (loop == Loop::Discard)
            self.Reset();
         return {result, loop};
      }

   protected:
      /// Iterate and execute call for each flat element, counting each       
      /// successfull execution                                               
      ///   @attention assumes block is typed and non empty                   
      ///   @tparam REVERSE whether to iterate in reverse                     
      ///   @param f the function to execute for each element of type A       
      ///   @param index [out] counts the successful executions               
      ///   @return the last 'f' result                                       
      template<bool REVERSE, CT::Container C, class F>
      LoopControl ForEachInner(this C&& self, F&& f, Count<C>& index) noexcept(IsNoexcept<F>) {
         AssumeDev(self.GetCount(), "Can't iterate empty container");
         AssumeDev(self.IsTyped(), "Can't iterate untyped container");

         using A  = ArgumentOf<F>;
         using R  = ReturnOf<F>;
         using DA = Decay<A>;
         constexpr bool TypeErased = Deref<C>::TypeErased;

         static_assert(CT::Slab<A> or CT::Constant<Deptr<A>> or CT::Mutable<C>,
            "Non-constant iterator for constant container is not allowed");

         LoopControl loop = Loop::NextLoop;

         if constexpr (not TypeErased) {
            // Container is not type-erased                             
            // Leverage compile-time optimizations                      
            using T  = TypeOf<C>;
            using DT = Decay<T>;

            if constexpr (CT::Deep<DA, DT> or (not CT::Deep<DA> and CT::DerivedFrom<T, A>)) {
               loop = self.template IterateInner<REVERSE>(
                  self.GetCount(),
                  [&index, &f](T& element) noexcept(IsNoexcept<F>) -> R {
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
         else if constexpr (not CT::DefineTag<DA>) {
            // Container is type-erased                                 
            // And we're NOT iterating using a tag                      
            if ((CT::Deep<DA> and self.IsDeep()) or (not CT::Deep<DA> and self.template CastsTo<A, true>())) {
               if (self.mType.IsSparse()) {
                  // Iterate sparse container                           
                  loop = self.template IterateInner<REVERSE>(
                     self.GetCount(),
                     [&index, &f](void*& element) noexcept(IsNoexcept<F>) -> R {
                        ++index;
                        if constexpr (CT::Dense<A>)
                           return f(*reinterpret_cast<Deref<A>*>(element));
                        else
                           return f( reinterpret_cast<A>(element));
                     }
                  );
               }
               else {
                  // Iterate dense container where A is binary-         
                  // compatible to the type, but may not be it exactly  
                  AssumeDev(self.GetStride() % sizeof(DA) == 0, "Unaligned iterator");
                  loop = self.template IterateInner<REVERSE>(
                     self.GetCount() * (self.GetStride() / sizeof(DA)),
                     [&index, &f](DA& element) noexcept(IsNoexcept<F>) -> R {
                        ++index;
                        if constexpr (CT::Dense<A>)
                           return f( element);
                        else
                           return f(&element);
                     }
                  );
               }
            }
         }
         else {
            // Container is type-erased                                 
            // And we're iterating using a tag                          
            using Identity = CT::ReflectedAs<DA>;
            if (not self.mType.template Is<Identity>())
               return Loop::NextLoop;

            // Container is type-erased and full of tags. Iterator is   
            // a static tag, so we iterate all tags visiting only       
            // those that match the definition in the argument          
            if (self.mType.IsSparse()) {
               // Iterate sparse container                              
               loop = self.template IterateInner<REVERSE>(
                  self.GetCount(),
                  [&index, &f](Identity*& element) noexcept(IsNoexcept<F>) -> R {
                     if constexpr (CT::Void<R>) {
                        if (not element->template IsTag<DA>())
                           return;
                     }
                     else if (not element->template IsTag<DA>())
                        return Loop::Continue;

                     ++index;

                     if constexpr (CT::Dense<A>)
                        return f(*reinterpret_cast<Deref<A>*>(element));
                     else
                        return f( reinterpret_cast<A>(element));
                  }
               );
            }
            else {
               // Iterate dense container                               
               loop = self.template IterateInner<REVERSE>(
                  self.GetCount(),
                  [&index, &f](Identity& element) noexcept(IsNoexcept<F>) -> R {
                     if constexpr (CT::Void<R>) {
                        if (not element.template IsTag<DA>())
                           return;
                     }
                     else if (not element.template IsTag<DA>())
                        return Loop::Continue;

                     ++index;
                     if constexpr (CT::Dense<A>)
                        return f(reinterpret_cast<Deref<A>&>( element));
                     else
                        return f(reinterpret_cast<Deref<A>*>(&element));
                  }
               );
            }
         }

         return loop;
      }
      
      /// Iterate and execute call for each deep element, counting each       
      /// successfull execution                                               
      ///   @tparam REVERSE whether to iterate in reverse                     
      ///   @tparam SKIP whether to execute call for intermediate blocks      
      ///   @param f the function to execute for each element of type A       
      ///   @param counter [out] counts the successful executions             
      ///   @return the last 'f' result                                       
      template<bool REVERSE, bool SKIP, CT::Container C, class F>
      LoopControl ForEachDeepInner(this C&& self, F&& f, Count<C>& counter) noexcept(IsNoexcept<F>) {
         using A = ArgumentOf<F>;
         using R = ReturnOf<F>;
         constexpr bool TypeErased = Deref<C>::TypeErased;
         [[maybe_unused]] LoopControl loop = Loop::Continue;

         static_assert(CT::Slab<A> or CT::Constant<Deptr<A>> or CT::Mutable<C>,
            "Non-constant iterator for constant container is not allowed");

         if constexpr (TypeErased) {
            const bool deep = self.IsDeep();
            using D = Deep<C>;

            if constexpr (CT::Deep<A>) {
               if (not SKIP or not deep) {
                  // Always execute for intermediate/non-deep *this     
                  ++counter;

                  decltype(auto) argument = self.template ReinterpretCast<A>();

                  if constexpr (CT::Bool<R>) {
                     if (not f(argument))
                        return Loop::Break;
                  }
                  else if constexpr (Exact<R, LoopControl>) {
                     // Do things depending on the F's return           
                     R loop = f(argument);

                     while (loop == Loop::Repeat)
                        loop = f(argument);

                     switch (loop.mControl) {
                     case LoopControl::Break:
                     case LoopControl::NextLoop:
                        return loop;
                     case LoopControl::Continue:
                     case LoopControl::Repeat:
                        break;
                     case LoopControl::Discard:
                        if constexpr (CT::Mutable<C>) {
                           // Discard is allowed only if THIS is mutable
                           // You can't fully discard the topmost block,
                           // only reset it. Now, if we reset this      
                           // block, and then remove it up the chain, if
                           // branching-out happens to occur, we'll end 
                           // up with a branch that contains the empty  
                           // element and that is bad. So defer the     
                           // reset up the chain instead!               
                           return Loop::Discard;
                        }
                        else {
                           // ...otherwise it acts like a Loop::Continue
                           break;
                        }
                     }
                  }
                  else f(argument);
               }
            }

            if (deep) {
               // Iterate subblocks                                     
               Count<C> intermediateCounterSink = 0;
               loop = self.template ForEachInner<REVERSE>(
                  [&counter, &f](D group) {
                     if constexpr (Akin<A, D>) {
                        // Loop control is available only if iterator   
                        // is deep, too...                              
                        return group.template ForEachDeepInner<REVERSE, SKIP>(
                           ::std::move(f), counter);
                     }
                     else {
                        // ... otherwise we have to pass through all    
                        // deep sub-blocks                              
                        group.template ForEachDeepInner<REVERSE, SKIP>(
                           ::std::move(f), counter);
                     }
                  },
                  intermediateCounterSink
               );
            }
            else if (self.template Is<Neat>()) {
               // Nest inside normalized subblocks                      
               using SubNeat = Tmut<C, Neat&, Neat const&>;

               loop = self.template ForEachInner<REVERSE>(
                  [&f](SubNeat neat) {
                     return neat.ForEachDeep(::std::move(f));
                  },
                  counter
               );
            }
            else if constexpr (not CT::Deep<A>) {
               // Equivalent to non-deep iteration                      
               loop = self.template ForEachInner<REVERSE>(
                  ::std::move(f), counter);
            }
         }
         else {
            using T = TypeOf<C>;

            if constexpr (CT::Deep<A> and (not SKIP or not CT::Deep<T>)) {
               // Always execute for intermediate/non-deep *this        
               ++counter;

               decltype(auto) argument = self.template ReinterpretCast<A>();

               if constexpr (CT::Bool<R>) {
                  if (not f(argument))
                     return Loop::Break;
               }
               else if constexpr (Exact<R, LoopControl>) {
                  // Do things depending on the F's return              
                  R loop = f(argument);

                  while (loop == Loop::Repeat)
                     loop = f(argument);

                  switch (loop.mControl) {
                  case LoopControl::Break:
                  case LoopControl::NextLoop:
                     return loop;
                  case LoopControl::Continue:
                  case LoopControl::Repeat:
                     break;
                  case LoopControl::Discard:
                     if constexpr (CT::Mutable<C>) {
                        // Discard is allowed only if THIS is mutable   
                        // You can't fully discard the topmost block,   
                        // only reset it. Now, if we reset this block,  
                        // and then remove it up the chain, if          
                        // branching-out happens to occur, we'll end up 
                        // with a branch that contains the empty element
                        // and that is bad. So defer the reset up the   
                        // chain instead!                               
                        return Loop::Discard;
                     }
                     else {
                        // ...otherwise it acts like a Loop::Continue   
                        break;
                     }
                  }
               }
               else f(argument);
            }

            if constexpr (CT::Deep<T>) {
               // Iterate subblocks                                     
               Count<C> intermediateCounterSink = 0;
               using SubBlock = Tmut<C, Decay<T>&, Decay<T> const&>;

               loop = self.template ForEachInner<REVERSE>(
                  [&counter, &f](SubBlock group) {
                     return group.template ForEachDeepInner<REVERSE, SKIP>(
                        ::std::move(f), counter);
                  },
                  intermediateCounterSink
               );
            }
            else if constexpr (Akin<T, Neat>) {
               // Iterate normalized subblocks                          
               using SubNeat = Tmut<C, Neat&, Neat const&>;

               loop = self.template ForEachInner<REVERSE>(
                  [&f](SubNeat neat) {
                     return neat.ForEachDeep(::std::move(f));
                  },
                  counter
               );
            }
            else if constexpr (not CT::Deep<A>) {
               // Equivalent to non-deep iteration                      
               loop = self.template ForEachInner<REVERSE>(
                  ::std::move(f), counter);
            }
         }

         return loop;
      }

      /// Execute a function for each element inside container                
      /// Lowest-level element iteration function (for internal use only)     
      ///   @attention assumes A is binary compatible with the contained type 
      ///   @attention assumes container is not empty                         
      ///   @attention assumes sparseness matches                             
      ///   @tparam REVERSE direction we're iterating in                      
      ///   @param f the constexpr noexcept function to call on each item     
      template<bool REVERSE, CT::Container C, class F>
      LoopControl IterateInner(this C&& self, Count<C> count, F&& f) noexcept(IsNoexcept<F>) {
         using A = ArgumentOf<F>;
         using R = ReturnOf<F>;

         static_assert(CT::Complete<Decay<A>> or CT::Sparse<A>,
            "Can't iterate with incomplete type, use pointer instead");

         AssumeDev(self.IsTyped(), 
            "Block is not typed");
         AssumeDev(not self.IsEmpty(), 
            "Block is empty (of type `", self.GetType(), "`)");
         AssumeDev(self.IsSparse() == CT::Sparse<A>,
            "Sparseness mismatch (`", self.GetType(),
            "` compared against `", MetaDataOf<A>(), "`)");

         if constexpr (CT::Dense<A>) {
            AssumeDev(self.template CastsTo<A, true>(),
               "Incompatible iterator type", " `", MetaDataOf<A>(), 
               "` (iterating block of type `", self.GetType(), "`)");
         }

         // Prepare for the loop                                        
         using DA = Deref<A>;
         auto raw = self.template GetRawAs<DA>();
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
            else if constexpr (Exact<R, LoopControl>) {
               // Do things depending on the F's return                 
               const R loop = f(*data);
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
                  if constexpr (CT::Mutable<C>) {
                     // Discard is allowed only if THIS is mutable      
                     // Why bother removing, when there's only one      
                     // element? Just propagate the discard instead!    
                     // The pack should be reset from above either way  
                     if (self.GetCount() == 1)
                        return Loop::Discard;

                     const Count<C> idx = raw - data;
                     self.RemoveAt(idx);

                     /*if (IsDeep() and mCount == 1) { //TODO this is quite experimental and not fully working right now. can be achieved by Optimize() after the loop for now
                        // Is only one element remaining? Is that element  
                        // deep? Then optimize this container away!        
                        auto temporary = GetDeep();
                        const_cast<Block*>(this)->GetDeep().ResetMemory();
                        const_cast<Block*>(this)->Free();
                        *const_cast<Block*>(this) = temporary;
                        return Loop::Repeat;
                     }
                     else {*/
                        // Block might BranchOut on RemoveIndex - make  
                        // sure 'raw', 'data' and 'dataEnd' are up-to-  
                        // date with new block memory                   
                        --count;
                        raw = self.template GetRawAs<DA>();
                        data = raw + idx;
                        dataEnd = REVERSE ? raw - 1 : raw + count;

                        if constexpr (REVERSE)
                           next();
                     //}
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
   };
}
