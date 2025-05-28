///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include <Langulus/Intent.hpp>
#include <Langulus/Assume.hpp>
#include <Langulus/CT/Support.hpp>


namespace Langulus
{
   
   /// Create an instance of T at the provided memory using placement new     
   /// which considers the intent and checks if T's constructors support it   
   /// All intent-related construction concepts are defined in terms of this  
   /// function. Beware, this is very unsafe - make sure all assumptions are  
   /// correct                                                                
   ///   @attention assumes placement pointer is valid and is of type T       
   ///   @attention when S is a deep intent (like Clone) this function        
   ///      assumes that the 'placement' pointer always points to an          
   ///      instance of type Decay<T>                                         
   ///   @param placement - where to place the new instance                   
   ///   @param value - the constructor argument, with or without intent      
   ///   @return the instance on the heap                                     
   template<bool FAKE = false, template<class> class S, CT::NoIntent T>
   requires CT::Intent<S<T>> LANGULUS(INLINED)
   constexpr auto IntentNew(void* placement, S<T>&& value) {
      constexpr auto TName = NameOf<T>();
      static_assert(CT::Complete<T>, TName +
         " has to be complete in order to be constructed");
      static_assert(not CT::Abstract<T>, TName +
         " has to be concrete in order to be constructed");
      static_assert(not CT::Reference<T>, TName +
         " can't be a reference in order to be constructed");

      AssumeDev(placement, HERE(), "Invalid placement pointer");

      if constexpr (CT::Referred<S<T>>) {
         // Refer                                                       
         if constexpr (CT::HasReferConstructor<T>)
            return new (placement) T (FWD(value));
         else if constexpr (CT::POD<T>) {
            ::std::memcpy(placement, (const void*) &*value, sizeof(T));
            return static_cast<T*>(placement);
         }
         else {
            static_assert(FAKE, "Can't refer-construct type: " + TName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Moved<S<T>>) {
         // Move                                                        
         if constexpr (CT::HasMoveConstructor<T>)
            return new (placement) T (FWD(value));
         else if constexpr (CT::POD<T>) {
            ::std::memmove(placement, (const void*) &*value, sizeof(T));
            return static_cast<T*>(placement);
         }
         else {
            static_assert(FAKE, "Can't move-construct type: " + TName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Abandoned<S<T>>) {
         // Abandon                                                     
         if constexpr (CT::HasAbandonConstructor<T>)
            return new (placement) T (FWD(value));
         else if constexpr (CT::POD<T>) {
            if constexpr (CT::HasMoveConstructor<T>)
               return new (placement) T (Move(*value));
            else {
               ::std::memmove(placement, (const void*) &*value, sizeof(T));
               return static_cast<T*>(placement);
            }
         }
         else {
            static_assert(FAKE,
               "Can't abandon-construct destructible type"
               " - explicit abandon-constructor is required for: " + TName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Cloned<S<T>>) {
         // Clone                                                       
         // @attention - assumes that all levels of indirection have    
         //    been allocated and pointers point to valid memory        
         using DT = Decay<T>;
         constexpr auto DTName = NameOf<DT>();

         //TODO nest for pointers

         if constexpr (not CT::Void<DT>) {
            if constexpr (CT::HasCloneConstructor<DT>)
               return new (placement) DT (Clone(DenseCast(*value)));
            else if constexpr (CT::POD<DT>) {
               if constexpr (::std::copy_constructible<DT>)
                  return new (placement) DT (DenseCast(*value));
               else {
                  ::std::memcpy(placement, (const void*) &*value, sizeof(DT));
                  return static_cast<T*>(placement);
               }
            }
            else {
               static_assert(FAKE, "Can't clone-construct type: " + DTName);
               return Unsupported {};
            }
         }
         else {
            static_assert(FAKE, "Can't clone-construct a void type: " + DTName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Copied<S<T>>) {
         // Copy                                                        
         if constexpr (CT::HasCopyConstructor<T>)
            return new (placement) T (FWD(value));
         else if constexpr (CT::POD<T>) {
            if constexpr (::std::copy_constructible<T>)
               return new (placement) T (*value);
            else {
               ::std::memcpy(placement, (const void*) &*value, sizeof(T));
               return static_cast<T*>(placement);
            }
         }
         else {
            static_assert(FAKE, "Can't copy-construct type: " + TName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Disowned<S<T>>) {
         // Disown                                                      
         if constexpr (CT::HasDisownConstructor<T>)
            return new (placement) T (FWD(value));
         else if constexpr (CT::POD<T>) {
            if constexpr (::std::copy_constructible<T>)
               return new (placement) T (*value);
            else {
               ::std::memcpy(placement, (const void*) &*value, sizeof(T));
               return static_cast<T*>(placement);
            }
         }
         else {
            static_assert(FAKE, "Can't disown-construct type: " + TName);
            return Unsupported {};
         }
      }
      else static_assert(false, "Intent wasn't recognized: " + NameOf<S<T>>());
   }

   /// Assign new value to an instance of T, using the provided intent        
   ///   @attention when S is a deep intent (like Clone) this function        
   ///      will DenseCast 'lhs' and 'rhs', and copy only dense data          
   ///   @param lhs - left hand side (what are we assigning to)               
   ///   @param rhs - right hand side (what are we assigning)                 
   ///   @return whatever the assignment operator returns                     
   template<bool FAKE = false, template<class> class S, CT::NoIntent T>
   requires CT::Intent<S<T>> LANGULUS(INLINED)
   constexpr decltype(auto) IntentAssign(T& lhs, S<T>&& rhs) {
      constexpr auto TName = NameOf<T>();
      static_assert(CT::Mutable<T>, TName +
         " has to be mutable in order to be assigned");
      static_assert(CT::Complete<T>, TName +
         " has to be complete in order to be assigned");
      static_assert(not CT::Reference<T>, TName +
         " can't be a reference in order to be assigned");

      if constexpr (CT::Referred<S<T>>) {
         // Refer                                                       
         if constexpr (CT::HasReferAssign<T>)
            return (lhs = Refer(rhs));
         else if constexpr (CT::POD<T>) {
            ::std::memcpy((void*) &lhs, (const void*) &*rhs, sizeof(T));
            return (lhs);
         }
         else {
            static_assert(FAKE, "Can't refer-assign type: " + TName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Moved<S<T>>) {
         // Move                                                        
         if constexpr (CT::HasIntentAssign<Langulus::Move, T>)
            return (lhs = Move(rhs));
         else if constexpr (::std::assignable_from<T&, T&&>)
            return (lhs = *rhs);
         else if constexpr (CT::POD<T>) {
            ::std::memmove((void*) &lhs, (const void*) &*rhs, sizeof(T));
            return (lhs);
         }
         else {
            static_assert(FAKE, "Can't move-assign type: " + TName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Abandoned<S<T>>) {
         // Abandon                                                     
         if constexpr (CT::HasAbandonAssign<T>)
            return (lhs = Abandon(rhs));
         else if constexpr (CT::HasReferAssign<T> and CT::HasAbandonConstructor<T>)
            // This is required because G++ doesn't detect implicit     
            // abandon-assignment otherwise                             
            return (lhs = Decvq<T> {FWD(rhs)});
         else if constexpr (CT::POD<T>) {
            if constexpr (CT::HasIntentAssign<Langulus::Move, T>)
               return (lhs = Move(*rhs));
            else if constexpr (::std::assignable_from<T&, T&&>)
               return (lhs = *rhs);
            else {
               ::std::memmove((void*) &lhs, (const void*) &*rhs, sizeof(T));
               return (lhs);
            }
         }
         else {
            static_assert(FAKE,
               "Can't abandon-assign destructible type"
               " - explicit abandon-assigner is required for: " + TName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Cloned<S<T>>) {
         // Clone                                                       
         // @attention - assumes that all levels of indirection have    
         //    been allocated and pointers point to valid memory        
         using DT = Decay<T>;
         constexpr auto DTName = NameOf<DT>();

         //TODO nest for pointers
         if constexpr (CT::Complete<DT> and not CT::Void<DT>) {
            if constexpr (CT::Mutable<Deptr<T>>) {
               if constexpr (CT::HasCloneAssign<DT>)
                  return (DenseCast(lhs) = Clone(DenseCast(*rhs)));
               else if constexpr (CT::POD<DT>) {
                  if constexpr (::std::assignable_from<DT&, const DT&>)
                     return (DenseCast(lhs) = DenseCast(*rhs));
                  else {
                     ::std::memcpy((void*) &lhs, (const void*) &*rhs, sizeof(DT));
                     return (lhs);
                  }
               }
               else {
                  static_assert(FAKE, "Can't clone-assign type: " + DTName);
                  return Unsupported {};
               }
            }
            else {
               static_assert(FAKE, "Can't clone-assign type - lhs is not mutable: " + DTName);
               return Unsupported {};
            }
         }
         else {
            static_assert(FAKE, "Can't clone-assign void or incomplete type: " + DTName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Copied<S<T>>) {
         // Copy                                                        
         if constexpr (CT::HasCopyAssign<T>)
            return (lhs = Copy(rhs));
         else if constexpr (CT::POD<T>) {
            if constexpr (::std::assignable_from<T&, const T&>)
               return (lhs = *rhs);
            else {
               ::std::memcpy((void*) &lhs, (const void*) &*rhs, sizeof(T));
               return (lhs);
            }
         }
         else {
            static_assert(FAKE, "Can't copy-assign type: " + TName);
            return Unsupported {};
         }
      }
      else if constexpr (CT::Disowned<S<T>>) {
         // Disown                                                      
         if constexpr (CT::HasDisownAssign<T>)
            return (lhs = Disown(rhs));
         else if constexpr (CT::POD<T>) {
            if constexpr (::std::assignable_from<T&, const T&>)
               return (lhs = *rhs);
            else {
               ::std::memcpy((void*) &lhs, (const void*) &*rhs, sizeof(T));
               return (lhs);
            }
         }
         else {
            static_assert(FAKE, "Can't disown-assign type: " + TName);
            return Unsupported {};
         }
      }
      else static_assert(false, "Intent wasn't recognized: " + NameOf<S<T>>());
   }

   namespace CT
   {
   
      /// Check if T is constructible with each of the provided arguments     
      ///   @attention that this differs from std::constructible_from, by     
      ///      attempting each argument separately                            
      ///   @attention this also includes aggregate type construction, so it  
      ///      will return true if first member is constructible with each A  
      template<class T, class...A>
      concept ConstructibleFrom = ((::std::constructible_from<T, A&&>) and ...);

      /// Check if T is assignable with each of the provided arguments        
      template<class T, class...A>
      concept AssignableFrom = (requires (T t, A&& a) { t = FWD(a); } and ...);


      ///                                                                     
      ///   Constructibles                                                    
      ///                                                                     
      ///   These concepts are bit looser on requirements, compared to their  
      /// Has*Constructor counterparts, to allow for fallbacks in places where
      /// they are required. A type may not explicitly HasAbandonConstructor, 
      /// and yet be AbandonConstructible, because it is movable by the usual 
      /// C++20 semantics                                                     
      ///                                                                     

      /// Check if all T are intent-constructible by intent S                 
      /// T can be intent-constructible even if not having the specific       
      /// constructor, as long as T and S are compatible with standard C++20  
      /// semantics                                                           
      ///   @tparam S - the intent                                            
      ///   @tparam T... - the types                                          
      template<template<class> class S, class...T>
      concept IntentConstructible = NotVoid<T...> and Intent<S<T>...>
          and (requires (S<T>&& a) {
             {IntentNew<true>(nullptr, FWD(a))} -> Supported;
          } and ...);

      /// Check if all TypeOf<S> are intent-constructible by intent S         
      /// T can be intent-constructible even if not having the specific       
      /// constructor, as long as T and S are compatible with standard C++20  
      /// semantics                                                           
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept IntentConstructibleAlt = Intent<S...>
          and (requires (S&& a) {
             {IntentNew<true>(nullptr, FWD(a))} -> Supported;
          } and ...);

      /// Check if all T are disown-constructible                             
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      /// If POD, T can be disown-constructible even if not having the        
      /// specific disown constructor, as long as T is std::copy_constuctible 
      template<class...T>
      concept DisownConstructible = (IntentConstructible<Langulus::Disown, T> and ...);

      /// Check if all Decay<T> are clone-constructible                       
      /// Does a deep copy. If POD, Decay<T> can be clone-constructible even  
      /// if not having the specific clone constructor, as long as T is       
      /// std::copy_constuctible                                              
      template<class...T>
      concept CloneConstructible = (IntentConstructible<Langulus::Clone, T> and ...);

      /// Check if all T are abandon-constructible                            
      /// Does a move but doesn't fully reset source as an optimization -     
      /// assuming that the abandoned instance is never going to be used in   
      /// more ways, than just calling the destructor. T can be               
      /// abandon-constructible even if not having the specific abandon       
      /// constructor, as long as it is std::move_constuctible                
      template<class...T>
      concept AbandonConstructible = (IntentConstructible<Langulus::Abandon, T> and ...);

      /// Check if all T are refer-constructible                              
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership. T can be refer-constructible as long as T is             
      /// std::copy_constuctible                                              
      template<class...T>
      concept ReferConstructible = (IntentConstructible<Langulus::Refer, T> and ...);
      
      /// Check if all T are copy-constructible                               
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      /// If POD, T can be copy-constructible even if not having the specific 
      /// shallow-copy constructor, as long as T is std::copy_constuctible    
      template<class...T>
      concept CopyConstructible = (IntentConstructible<Langulus::Copy, T> and ...);

      /// Check if all T are move-constructible                               
      /// Does a move, fully resetting source into a reusable state           
      /// T is move-constructible as long as it is std::move_constuctible     
      template<class...T>
      concept MoveConstructible = (IntentConstructible<Langulus::Move, T> and ...);


      /// Check if all T are intent-assignable by intent S                    
      /// T can be intent-assignable even if not having an explicit assigner, 
      /// as long as T and S are compatible with the usual C++20 semantics    
      ///   @tparam S - the intent                                            
      ///   @tparam T... - the types                                          
      template<template<class> class S, class...T>
      concept IntentAssignable = NotVoid<T...> and Intent<S<T>...>
          and (requires (T t, S<T>&& a) {
            {IntentAssign<true>(t, FWD(a))} -> Supported;
          } and ...);

      /// Check if all TypeOf<S> are intent-assignable by S                   
      /// T can be intent-assignable even if not having an explicit assigner  
      /// as long as T and S are compatible with standard C++20 semantics     
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept IntentAssignableAlt = Intent<S...>
          and (requires (Decq<Deref<TypeOf<S>>> t, S&& a) {
            {IntentAssign<true>(t, FWD(a))} -> Supported;
          } and ...);

      /// Check if all T are disown-assignable                                
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      /// If POD, T can be disown-assignable even if not having an explicit   
      /// disown-assignment, as long as std::copy_assignable<T> holds         
      template<class...T>
      concept DisownAssignable = (IntentAssignable<Langulus::Disown, T> and ...);

      /// Check if all Decay<T> are clone-assignable                          
      /// Does a deep copy                                                    
      /// If POD, Decay<T> can be clone-assignable even if not having an      
      /// explicit clone-assignment, as long as std::copy_assignable<T> holds 
      template<class...T>
      concept CloneAssignable = (IntentAssignable<Langulus::Clone, T> and ...);

      /// Check if all T are abandon-assignable                               
      /// Does a move, but doesn't fully reset source (optimization)          
      /// T can be abandon-assignable even if not having an explicit          
      /// abandon-assignment, as long as std::move_assignable<T> holds        
      template<class...T>
      concept AbandonAssignable = (IntentAssignable<Langulus::Abandon, T> and ...);

      /// Check if all T are refer-assignable                                 
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership.                                                          
      /// T can be refer-assignable as long as std::copy_assignable<T> holds  
      template<class...T>
      concept ReferAssignable = (IntentAssignable<Langulus::Refer, T> and ...);
      
      /// Check if all T are copy-assignable                                  
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      /// If POD, T can be copy-assignable even if not having an explicit     
      /// copy-assigner, as long as std::copy_assignable<T> holds             
      template<class...T>
      concept CopyAssignable = (IntentAssignable<Langulus::Copy, T> and ...);

      /// Check if all T are move-assignable                                  
      /// Does a move, fully resetting source                                 
      /// T is move-assignable as long as std::move_assignable<T> holds       
      /// @attention you can't have move semantics, if a type has its         
      ///   destructor deleted - every time you move an instance, the old one 
      ///   has to be deleted later.                                          
      template<class...T>
      concept MoveAssignable = (IntentAssignable<Langulus::Move, T> and ...);

   } // namespace Langulus::CT

} // namespace Langulus
