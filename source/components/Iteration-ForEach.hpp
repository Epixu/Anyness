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

      constexpr bool operator == (const LoopControl& rhs) const noexcept {
         return mControl == rhs.mControl;
      }
   };

   namespace Loop
   {

      constexpr LoopControl Break      = LoopControl::Break;
      constexpr LoopControl Continue   = LoopControl::Continue;
      constexpr LoopControl Repeat     = LoopControl::Repeat;
      constexpr LoopControl Discard    = LoopControl::Discard;
      constexpr LoopControl NextLoop   = LoopControl::NextLoop;

   } // namespace Langulus::Loop

} // namespace Langulus

namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements ForEach iteration interface for containers                  
   ///                                                                        
   struct IterationForEach {
      using CTTI_Component = Yes;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;
      
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
      ///   @param calls - all potential functions to iterate with            
      ///   @return the number of executions and the control end code         
      template<CT::Container C, class...F>
      auto ForEach(this C&& self, F&&...lambda) -> ForEachResult<C> {
         static_assert(sizeof...(F) > 0, "No functions in ForEach");
         if (self.IsEmpty())
            return {};

         LoopControl loop = Loop::Break;
         Count<C> result = 0;
         (void)(... or (Loop::NextLoop != (
            loop = self.template ForEachInner<false>(::std::forward<F>(lambda), result)
         )));

         if (loop == Loop::Discard)
            self.Reset();
         return {result, loop};
      }

      /// Do it in reverse                                                    
      template<CT::Container C, class...F>
      auto ForEachRev(this C&& self, F&&...lambda) -> ForEachResult<C> {
         static_assert(sizeof...(F) > 0, "No functions in ForEach");
         if (self.IsEmpty())
            return {};

         LoopControl loop = Loop::Break;
         Count<C> result = 0;
         (void)(... or (Loop::NextLoop != (
            loop = self.template ForEachInner<true>(::std::forward<F>(lambda), result)
         )));

         if (loop == Loop::Discard)
            self.Reset();
         return {result, loop};
      }

      template<CT::Container C>
      auto ForEachDeep(this C&&, auto&&...) -> ForEachResult<C>;
      template<CT::Container C>
      auto ForEachDeepRev(this C&&, auto&&...) -> ForEachResult<C>;

      template<CT::Container C>
      auto ForEachDeepNoskip(this C&&, auto&&...) -> ForEachResult<C>;
      template<CT::Container C>
      auto ForEachDeepNoskipRev(this C&&, auto&&...) -> ForEachResult<C>;

   protected:
      /// Iterate and execute call for each flat element, counting each       
      /// successfull execution                                               
      ///   @attention assumes block is typed and non empty                   
      ///   @tparam REVERSE - whether to iterate in reverse                   
      ///   @param f - the function to execute for each element of type A     
      ///   @param index - [out] counts the successful executions             
      ///   @return the last 'f' result                                       
      template<bool REVERSE, CT::Container C, class F>
      LoopControl ForEachInner(this C&& self, F&& f, Count<C>& index) noexcept(IsNoexcept<F>) {
         AssumeDev(self.GetCount(), HERE(), "Can't iterate empty container");
         AssumeDev(self.IsTyped(),  HERE(), "Can't iterate untyped container");

         using A  = ArgumentOf<F>;
         using R  = ReturnOf<F>;
         using DA = Decay<A>;
         constexpr bool TypeErased = Deref<C>::TypeErased;

         static_assert(CT::Slab<A> or CT::Constant<Deptr<A>> or CT::Mutable<C>,
            "Non-constant iterator for constant container is not allowed");

         LoopControl loop = Loop::NextLoop;

         if constexpr (not TypeErased) {
            // Container is not type-erased                             
            // Leverage any compile-time optimizations                  
            using T  = TypeOf<C>;
            using DT = Decay<T>;

            if constexpr (CT::Deep<DA, DT> or (not CT::Deep<DA> and CT::DerivedFrom<T, A>)) {
               loop = self.template IterateInner<REVERSE>(self.GetCount(),
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
                  loop = self.template IterateInner<REVERSE>(self.GetCount(),
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
                  AssumeDev(self.GetStride() % sizeof(DA) == 0, HERE(), "Unaligned iterator");
                  loop = self.template IterateInner<REVERSE>(self.GetCount() * (self.GetStride() / sizeof(DA)),
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
               loop = self.template IterateInner<REVERSE>(self.GetCount(),
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
               loop = self.template IterateInner<REVERSE>(self.GetCount(),
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
      
      /// Execute a function for each element inside container                
      /// Lowest-level element iteration function (for internal use only)     
      ///   @attention assumes A is binary compatible with the contained type 
      ///   @attention assumes container is not empty                         
      ///   @attention assumes sparseness matches                             
      ///   @tparam REVERSE - direction we're iterating in                    
      ///   @param call - the constexpr noexcept function to call on each item
      template<bool REVERSE, CT::Container C, class F>
      LoopControl IterateInner(this C&& self, Count<C> count, F&& f) noexcept(IsNoexcept<F>) {
         using A = ArgumentOf<F>;
         using R = ReturnOf<F>;

         static_assert(CT::Complete<Decay<A>> or CT::Sparse<A>,
            "Can't iterate with incomplete type, use pointer instead");

         AssumeDev(self.IsTyped(), HERE(),
            "Block is not typed");
         AssumeDev(not self.IsEmpty(), HERE(),
            "Block is empty (of type `", self.GetType(), "`)");
         AssumeDev(self.IsSparse() == CT::Sparse<A>, HERE(),
            "Sparseness mismatch (`", self.GetType(),
            "` compared against `", MetaDataOf<A>(), "`)");

         if constexpr (CT::Dense<A>) {
            AssumeDev(self.template CastsTo<A, true>(), HERE(),
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
            else if constexpr (CT::Exact<R, LoopControl>) {
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

} // namespace Langulus::Anyness::Component
