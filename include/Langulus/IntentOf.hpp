///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "TypeOf.hpp"
#include "Assume.hpp"
#include "CT/Derived.hpp"
#include "CT/POD.hpp"
#include "CT/Support.hpp"
#include <new>


namespace Langulus::CTTI
{

   /// Affects CT::Intent                                                     
   template<class T>
   struct Intent {
      static constexpr bool Enabled = false;
   };
   
} // namespace Langulus::CTTI

namespace Langulus::CT
{

   /// Check if all T are sheddable intents                                   
   template<class...T>
   concept Intent = Inner::CheckSize<T...>() and (
         (CTTI::Intent<Deref<T>>::Enabled or LANGULUS_CTTI_DELVE_IN(T, Intent)
      ) and ...);

   /// Check if all T are NOT sheddable intents                               
   template<class...T>
   concept NoIntent = Inner::CheckSize<T...>()
       and ((not Intent<Deref<T>>) and ...);
   

   ///                                                                        
   /// All intents are defined in terms of three properties, and the          
   /// combinations between them:                                             
   ///   unsigned Depth - decides whether the intent is deep or shallow       
   ///   bool     Keep  - decides whether to exercise ownership or not        
   ///   bool     Move  - decides whether it's a move semantic or not         

   /// Checks if all T are shallow intents                                    
   /// Shallow intents are propagated through mostly a single indirection     
   template<class...T>
   concept ShallowIntent = Inner::CheckSize<T...>()
       and ((Intent<Deref<T>> and Decay<T>::IsShallow()) and ...);

   /// Checks if all T are deep intents                                       
   /// Deep intents propagate through all levels of indirection               
   template<class...T>
   concept DeepIntent = Inner::CheckSize<T...>()
       and ((Intent<Deref<T>> and not Decay<T>::IsShallow()) and ...);

   /// Check if all T are refer intents                                       
   /// Does a shallow-copy without delving into any indirections, while       
   /// exercising ownership of managed data                                   
   template<class...T>
   concept Referred = Inner::CheckSize<T...>()
       and ((Intent<Deref<T>> and Decay<T>::Is(0, true, false)) and ...);
      
   /// Check if all T are copy intents                                        
   /// Does a shallow-copy, while cloning only the first indirection level    
   template<class...T>
   concept Copied = Inner::CheckSize<T...>()
       and ((Intent<Deref<T>> and Decay<T>::Is(1, true, false)) and ...);

   /// Check if all T are move intents                                        
   /// Moves by leaving the moved instances reusable                          
   template<class...T>
   concept Moved = Inner::CheckSize<T...>()
       and ((Intent<Deref<T>> and Decay<T>::Is(0, true, true)) and ...);

   /// Check if all T are abandon intents                                     
   /// Moves by leaving the moved instances no longer usable                  
   template<class...T>
   concept Abandoned = Inner::CheckSize<T...>()
       and ((Intent<Deref<T>> and Decay<T>::Is(0, false, true)) and ...);

   /// Check if all T are disown intents                                      
   /// Does a shallow-copy without delving into any indirections, without     
   /// exercising any ownership                                               
   template<class...T>
   concept Disowned = Inner::CheckSize<T...>()
       and ((Intent<Deref<T>> and Decay<T>::Is(0, false, false)) and ...);

   /// Check if all T are clone intents                                       
   /// Does a deep-copy throughout all levels of indirection                  
   template<class...T>
   concept Cloned = Inner::CheckSize<T...>()
       and ((DeepIntent<Deref<T>> and Decay<T>::Is(true, false)) and ...);

} // namespace Langulus::CT

namespace Langulus
{
   
   /// Shed only the intent from a type, if any                               
   template<class T>
   using Deint = Tif<CT::Intent<Deref<T>>, TypeOf<T>, T>;

   /// This just makes sure that mutable references are forwarded properly    
   /// by attaching a deprecation warning to it                               
   template<CT::Mutable T>
   DEBUGGERY([[deprecated("Make sure you forward the argument")]])
   LANGULUS(ALWAYS_INLINED)   
   constexpr decltype(auto) DeintCast(T& what) {
      if constexpr (CT::Intent<T>) return *what;
      else return (what);
   }

   /// Decay an intent to the contained data                                  
   ///   @param what - the instance to decay                                  
   ///   @return a reference (preferably) or a copy of the inner data         
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr decltype(auto) DeintCast(T&& what) noexcept {
      if constexpr (CT::Intent<T>) return *what;
      else return FWD(what);
   }

   namespace Inner
   {

      /// Helper base that defines intent properties                          
      ///   @tparam DEPTH - the depth of the intent, use -1 for infinite      
      ///   @tparam KEEP - does the intent practice ownership                 
      ///   @tparam MOVE - does the intent involve transfer of ownership      
      template<unsigned DEPTH, bool KEEP, bool MOVE>
      struct CommonIntent {
         using CTTI_ReflectAs     = void;
         using CTTI_Abstract      = Yes;
         using CTTI_Unallocatable = Yes;
         using CTTI_Intent        = Yes;
         using CTTI_Sheddable     = Yes;

         static consteval unsigned GetDepth() { return DEPTH; }
         static consteval bool IsKept()       { return KEEP;  }
         static consteval bool IsMoved()      { return MOVE;  }
         static consteval bool ResetsOnMove() { return KEEP and MOVE; }
         static consteval bool IsShallow()    { return DEPTH < 2;     }
         static consteval bool Is(int depth, bool keep, bool move) {
            return DEPTH == depth and KEEP == keep and MOVE == move;
         }
         static consteval bool Is(bool keep, bool move) {
            return KEEP == keep and MOVE == move;
         }
      };

   } // namespace Langulus::Inner


   ///                                                                        
   /// Referred value intermediate type, use in constructors and assignments  
   /// to refer to data explicitly                                            
   ///   @tparam T - the type to refer                                        
   template<class T>
   struct Refer final : Inner::CommonIntent<0, true, false> {
   private:
      const T& mValue;

   public:
      using CTTI_Typed = decltype(mValue);

      template<class ALT>
      using Retype = Refer<Decq<Deref<Deint<ALT>>>>;

      Refer() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Refer(const T& value) noexcept : mValue {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      template<CT::Intent I> LANGULUS(ALWAYS_INLINED)
      explicit constexpr Refer(I&& value) noexcept : mValue {value.mValue} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      /// Forward as referred                                                 
      ///   @tparam ALT_T - optional type to forward as                       
      ///   @return the desired new type with the same refer intent applied   
      template<class ALT_T = T> LANGULUS(ALWAYS_INLINED)
      constexpr decltype(auto) Forward() const noexcept {
         static_assert(CT::NoIntent<ALT_T>,
            "Can't nest intents");
         static_assert(CT::DerivedFrom<T, ALT_T>,
            "Can't forward as this type");

         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return static_cast<const ALT_T&>(mValue);
         else
            return Refer<ALT_T> (static_cast<const ALT_T&>(mValue));
      }

      /// Refer something else                                                
      ///   @param value - the value to refer (can be an intent)              
      ///   @return the referred value, disregarding previous intent          
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast(value);
         else
            return Retype<ALT_T> (DeintCast(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T* operator -> () const noexcept { return SparseCast(mValue); }

      /// Implicitly collapse the intent in some cases                        
      /// This way the wrapper is seamlessly integrated with the standard     
      /// C++20 copy semantics                                                
      //LANGULUS(ALWAYS_INLINED)
      //explicit constexpr operator const T& () const noexcept { return mValue; }
   };

   template<CT::NoIntent T>
   Refer(T&) -> Refer<T>;

   template<CT::Intent T>
   Refer(T&&) -> Refer<Decq<Deref<TypeOf<T>>>>;
   
   
   ///                                                                        
   /// Copied value intermediate type, use in constructors and assignments    
   /// to shallow-copy container explicitly                                   
   ///   @tparam T - the type to copy                                         
   template<class T>
   struct Copy final : Inner::CommonIntent<1, true, false> {
   private:
      const T& mValue;

   public:
      using CTTI_Typed = decltype(mValue);

      template<class ALT>
      using Retype = Copy<Decq<Deref<Deint<ALT>>>>;

      Copy() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Copy(const T& value) noexcept : mValue {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      template<CT::Intent I> LANGULUS(ALWAYS_INLINED)
      explicit constexpr Copy(I&& value) noexcept : mValue {value.mValue} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      /// Forward as copied                                                   
      ///   @tparam ALT_T - optional type to forward as                       
      ///   @return the desired new type with the same copy intent applied    
      template<class ALT_T = T> LANGULUS(ALWAYS_INLINED)
      constexpr decltype(auto) Forward() const noexcept {
         static_assert(CT::NoIntent<ALT_T>,
            "Can't nest intents");
         static_assert(CT::DerivedFrom<T, ALT_T>,
            "Can't forward as this type");

         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return static_cast<const ALT_T&>(mValue);
         else
            return Copy<ALT_T> {mValue};
      }

      /// Copy something else                                                 
      ///   @param value - the value to copy (can be an intent)               
      ///   @return the copied value, disregarding previous intent            
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast(value);
         else
            return Retype<ALT_T> (DeintCast(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T* operator -> () const noexcept { return SparseCast(mValue); }
   };

   template<CT::NoIntent T>
   Copy(T&) -> Copy<T>;
   
   template<CT::Intent T>
   Copy(T&&) -> Copy<Decq<Deref<TypeOf<T>>>>;


   ///                                                                        
   /// Moved value intermediate type, use in constructors and assignments     
   /// to move data explicitly                                                
   ///   @tparam T - the type to move                                         
   template<class T>
   struct Move final : Inner::CommonIntent<0, true, true> {
   protected:
      static_assert(CT::Mutable<T>, "Constant T isn't movable");
      T&& mValue;

   public:
      using CTTI_Typed = decltype(mValue);

      template<class ALT>
      using Retype = Move<Decq<Deref<Deint<ALT>>>>;

      Move() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Move(T& value) noexcept : mValue {MOV(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Move(T&& value) noexcept : mValue {FWD(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      template<CT::Intent I> LANGULUS(ALWAYS_INLINED)
      explicit constexpr Move(I&& value) noexcept : mValue {FWD(value.mValue)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      /// Forward as moved                                                    
      ///   @tparam ALT_T - optional type to forward as                       
      ///   @return the desired new type with the same move intent applied    
      template<class ALT_T = T> LANGULUS(ALWAYS_INLINED)
      constexpr decltype(auto) Forward() const noexcept {
         static_assert(CT::NoIntent<ALT_T>,
            "Can't nest intents");
         static_assert(CT::DerivedFrom<T, ALT_T>,
            "Can't forward as this type");
         
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard move semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return static_cast<ALT_T&&>(mValue);
         else
            return Move<ALT_T> {static_cast<ALT_T&&>(mValue)};
      }

      /// Move something else                                                 
      ///   @param value - the value to move (can be an intent)               
      ///   @return the moved value, disregarding previous intent             
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast(FWD(value));
         else
            return Retype<ALT_T> (DeintCast(FWD(value)));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr T&& operator * () const noexcept { return FWD(mValue); }

      LANGULUS(ALWAYS_INLINED)
      constexpr T* operator -> () const noexcept { return SparseCast(mValue); }

      /// Implicitly collapse the intent in some cases                        
      /// This way the wrapper is seamlessly integrated with the standard     
      /// C++20 move semantics                                                
      //LANGULUS(ALWAYS_INLINED)
      //explicit constexpr operator T&& () noexcept { return FWD(mValue); }
   };

   template<CT::NoIntent T>
   Move(T&&) -> Move<Deref<T>>;

   template<CT::Intent T>
   Move(T&&) -> Move<Decq<Deref<TypeOf<T>>>>;


   ///                                                                        
   /// Abandoned value intermediate type, can be used in constructors and     
   /// assignments to provide a guarantee, that the value shall not be used   
   /// after being consumed, so we can save up on resetting it fully          
   /// For example, you can construct a Many with an abandoned Many, which is 
   /// same as move-construction, but the abandoned Many shall have only its  
   /// mEntry reset, instead of the entire container, leaving it in a state   
   /// that is unfit for reuse, but also saving a lot of instructions.        
   ///   @tparam T - the type to abandon                                      
   template<class T>
   struct Abandon final : Inner::CommonIntent<0, false, true> {
   protected:
      static_assert(CT::Mutable<T>, "Constant T isn't abandonable");
      T&& mValue;

   public:
      using CTTI_Typed = decltype(mValue);

      template<class ALT>
      using Retype = Abandon<Decq<Deref<Deint<ALT>>>>;

      Abandon() = delete;
      explicit constexpr Abandon(Abandon const&) noexcept = default;
      explicit constexpr Abandon(Abandon&&) noexcept = default;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Abandon(T& value) noexcept : mValue {MOV(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Abandon(T&& value) noexcept : mValue {FWD(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      template<CT::Intent I> LANGULUS(ALWAYS_INLINED)
      explicit constexpr Abandon(I&& value) noexcept : mValue {FWD(value.mValue)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      /// Forward as abandoned                                                
      ///   @tparam ALT_T - optional type to forward as                       
      ///   @return the desired new type with the same move intent applied    
      template<class ALT_T = T> LANGULUS(ALWAYS_INLINED)
      constexpr decltype(auto) Forward() const noexcept {
         static_assert(CT::NoIntent<ALT_T>,
            "Can't nest intents");
         static_assert(CT::DerivedFrom<T, ALT_T>,
            "Can't forward as this type");
         
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard move semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return static_cast<ALT_T&&>(mValue);
         else
            return Abandon<ALT_T> {static_cast<ALT_T&&>(mValue)};
      }

      /// Abandon something else                                              
      ///   @param value - the value to abandon (can be an intent)            
      ///   @return the abandoned value, disregarding previous intent         
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast(FWD(value));
         else
            return Retype<ALT_T> (DeintCast(FWD(value)));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr T&& operator * () const noexcept { return FWD(mValue); }

      LANGULUS(ALWAYS_INLINED)
      constexpr T* operator -> () const noexcept { return SparseCast(mValue); }

      /// Implicitly collapse the intent in some cases                        
      /// This way the wrapper is seamlessly integrated with the standard     
      /// C++20 move semantics                                                
      //LANGULUS(ALWAYS_INLINED)
      //explicit constexpr operator T&& () const noexcept{ return FWD(mValue); }
   };
   
   template<CT::NoIntent T>
   Abandon(T&&) -> Abandon<Deref<T>>;

   template<CT::Intent T>
   Abandon(T&&) -> Abandon<Decq<Deref<TypeOf<T>>>>;


   ///                                                                        
   /// Disowned value intermediate type, use in constructors and assignments  
   /// to copy container without gaining ownership                            
   ///   @tparam T - the type to disown                                       
   template<class T>
   struct Disown final : Inner::CommonIntent<0, false, false> {
   protected:
      const T& mValue;

   public:
      using CTTI_Typed = decltype(mValue);

      template<class ALT>
      using Retype = Disown<Decq<Deref<Deint<ALT>>>>;

      Disown() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Disown(const T& value) noexcept : mValue {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      template<CT::Intent I> LANGULUS(ALWAYS_INLINED)
      explicit constexpr Disown(I&& value) noexcept : mValue {value.mValue} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      /// Forward as disowned                                                 
      ///   @tparam ALT_T - optional type to forward as                       
      ///   @return the desired new type with the same disown intent applied  
      template<class ALT_T = T> LANGULUS(ALWAYS_INLINED)
      constexpr decltype(auto) Forward() const noexcept {
         static_assert(CT::NoIntent<ALT_T>,
            "Can't nest intents");
         static_assert(CT::DerivedFrom<T, ALT_T>,
            "Can't forward as this type");

         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return static_cast<const ALT_T&>(mValue);
         else
            return Disown<ALT_T> {static_cast<const ALT_T&>(mValue)};
      }

      /// Disown something else                                               
      ///   @param value - the value to disown (can be an intent)             
      ///   @return the disowned value, disregarding previous intent          
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast(value);
         else
            return Retype<ALT_T> (DeintCast(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T* operator -> () const noexcept { return SparseCast(mValue); }
   };
   
   template<CT::NoIntent T>
   Disown(T&) -> Disown<T>;

   template<CT::Intent T>
   Disown(T&&) -> Disown<Decq<Deref<TypeOf<T>>>>;


   ///                                                                        
   /// Cloned value intermediate type, used in constructors and assignments   
   /// to clone container, doing a deep copy instead of default shallow one   
   ///   @tparam T - the type to clone                                        
   template<class T>
   struct Clone final : Inner::CommonIntent<static_cast<unsigned>(-1), true, false> {
   protected:
      const T& mValue;

   public:
      using CTTI_Typed = decltype(mValue);

      template<class ALT>
      using Retype = Clone<Decq<Deref<Deint<ALT>>>>;

      Clone() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Clone(const T& value) noexcept : mValue {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      template<CT::Intent I> LANGULUS(ALWAYS_INLINED)
      explicit constexpr Clone(I&& value) noexcept : mValue {value.mValue} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      /// Forward as cloned, never collapse                                   
      template<class ALT_T = T> LANGULUS(ALWAYS_INLINED)
      constexpr decltype(auto) Forward() const noexcept {
         static_assert(CT::NoIntent<ALT_T>,
            "Can't nest intents");
         static_assert(CT::DerivedFrom<T, ALT_T>,
            "Can't forward as this type");
         return Clone<ALT_T> {mValue};
      }

      /// Clone something else                                                
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         return Retype<ALT_T> (DeintCast(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T* operator -> () const noexcept { return SparseCast(mValue); }
   };
   
   template<CT::NoIntent T>
   Clone(T&) -> Clone<T>;

   template<CT::Intent T>
   Clone(T&&) -> Clone<Decq<Deref<TypeOf<T>>>>;


   namespace CT
   {

      ///                                                                     
      ///   Intent type traits                                                
      ///                                                                     
      /// These concepts are strict requirements, and are true only if the    
      /// corresponding constructors/assigners are defined. No fallbacks!     
      ///                                                                     

      /// Check if all T have dedicated intent constructors for S             
      ///   @tparam S - the intent                                            
      ///   @tparam T... - the types                                          
      template<template<class> class S, class...T>
      concept HasIntentConstructor = Intent<S<T>...> and not Aggregate<T...> and ((
            /*not ::std::is_trivially_constructible_v<T, TypeOf<S<T>>>
            and*/ requires (S<T>&& arg) { T {FWD(arg)}; }//::std::is_constructible_v<T, S<T>>
         ) and ...);
          //and requires (S<T>&&...a) { (T {FWD(a)}, ...); };

      /// Check if all TypeOf<S> have a dedicated intent constructor for S    
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept HasIntentConstructorAlt = Intent<S...> and not Aggregate<TypeOf<S>...> and ((
            /*not ::std::is_trivially_constructible_v<Decvq<Deref<TypeOf<S>>>, TypeOf<S>>
            and*/ requires (S&& arg) { Decvq<Deref<TypeOf<S>>> {FWD(arg)}; }//::std::is_constructible_v<Decvq<Deref<TypeOf<S>>>, S>
         ) and ...);
          //and requires (S&&...a) { (Decvq<Deref<TypeOf<S>>> {FWD(a)}, ...); };

      /// Check if all T have a dedicated disown-constructor                  
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      template<class...T>
      concept HasDisownConstructor = Inner::CheckSize<T...>()
          and (HasIntentConstructor<::Langulus::Disown, T> and ...);

      /// Check if all Decay<T> have a dedicated clone-constructor            
      /// Does a deep copy                                                    
      template<class...T>
      concept HasCloneConstructor = Inner::CheckSize<T...>()
          and (HasIntentConstructor<::Langulus::Clone, T> and ...);

      /// Check if all T have a dedicated abandon-constructor                 
      /// Does a move, but doesn't fully reset source (used for optimization) 
      template<class...T>
      concept HasAbandonConstructor = Inner::CheckSize<T...>()
          and (HasIntentConstructor<::Langulus::Abandon, T> and ...);

      /// Check if all T have a dedicated refer-constructor                   
      /// Refering does a shallow copy while referencing contents             
      template<class...T>
      concept HasReferConstructor = Inner::CheckSize<T...>()
          and (HasIntentConstructor<::Langulus::Refer, T> and ...);
      
      /// Check if all T have a dedicated copy-constructor                    
      /// Does a shallow copy _of the contents_ (it is like shallow cloning)  
      ///   @attention don't mistake it for the built-in copy-semantic        
      template<class...T>
      concept HasCopyConstructor = Inner::CheckSize<T...>()
          and (HasIntentConstructor<::Langulus::Copy, T> and ...);

      /// Check if all T have a dedicated move-constructor                    
      /// Does a move, fully resetting source                                 
      template<class...T>
      concept HasMoveConstructor = Inner::CheckSize<T...>()
          and (HasIntentConstructor<::Langulus::Move, T> and ...);

      /// Check if all T have a dedicated intent-assigner for S               
      ///   @tparam S - the intent                                            
      ///   @tparam T... - the types                                          
      template<template<class> class S, class...T>
      concept HasIntentAssign = Inner::CheckSize<T...>() and ((Intent<S<T>>
          /*and not ::std::is_trivially_assignable_v<T&, TypeOf<S<T>>>*/
          and requires (T& lhs, S<T>&& rhs) { lhs = FWD(rhs); } //    ::std::is_assignable_v<T&, S<T>>
         ) and ...);

      /// Check if all TypeOf<S> habe a dedicated intent-assigner for S       
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept HasIntentAssignAlt = Inner::CheckSize<S...>() and ((Intent<S>
          /*and not ::std::is_trivially_assignable_v<Decvq<Deref<TypeOf<S>>>&, TypeOf<S>>*/
          and requires (Decvq<Deref<TypeOf<S>>>& lhs, S&& rhs) { lhs = FWD(rhs); } //::std::is_assignable_v<Decvq<Deref<TypeOf<S>>>&, S>
         ) and ...);

      /// Check if all T have a dedicated disown-assigner                     
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      template<class...T>
      concept HasDisownAssign = Inner::CheckSize<T...>()
          and (HasIntentAssign<::Langulus::Disown, T> and ...);

      /// Check if all Decay<T> have a dedicated clone-assigner               
      /// Does a deep copy                                                    
      template<class...T>
      concept HasCloneAssign = Inner::CheckSize<T...>()
          and (HasIntentAssign<::Langulus::Clone, T> and ...);

      /// Check if all T have a dedicated abandon-assigner                    
      /// Does a move, but doesn't fully reset source (optimization)          
      template<class...T>
      concept HasAbandonAssign = Inner::CheckSize<T...>()
          and (HasIntentAssign<::Langulus::Abandon, T> and ...);

      /// Check if all T have a dedicated refer-assigner                      
      /// Refering does a shallow copy while referencing contents             
      template<class...T>
      concept HasReferAssign = Inner::CheckSize<T...>()
          and (HasIntentAssign<::Langulus::Refer, T> and ...);
      
      /// Check if all T have a dedicated copy-assigner                       
      /// Does a shallow copy _of the contents_ (it is like shallow cloning)  
      ///   @attention don't mistake it for the built-in copy-semantic        
      template<class...T>
      concept HasCopyAssign = Inner::CheckSize<T...>()
          and (HasIntentAssign<::Langulus::Copy, T> and ...);

      /// Check if all T have a dedicated move-assigner                       
      /// Does a move, fully resetting source                                 
      template<class...T>
      concept HasMoveAssign = Inner::CheckSize<T...>()
          and (HasIntentAssign<::Langulus::Move, T> and ...);

   } // namespace Langulus::CT

   
   /// Deduce the proper intent, based on whether T already has a             
   /// specified intent (like when it is an rvalue (&&))                      
   ///   - if it has one of those, then we get move intent (which can         
   ///     implicitly fallback to standard move semantics);                   
   ///   - if it isn't - we get refer intent (which in turn can fallback to   
   ///     standard copy semantics)                                           
   template<class T>
   using IntentOf = Tif<CT::Intent<Decvq<Deref<T>>>,
         Decvq<Deref<T>>,
         Tif<::std::is_rvalue_reference_v<T> and CT::Mutable<Deref<T>>,
            Move<Deref<T>>,
            Refer<Deref<T>>
         >
      >;

} // namespace Langulus


/// A handy constructor & assignment pattern that adds all possible intents   
/// and collapses them for a given type. Useful when you don't want intents   
/// to get in the way of simple types that need those reflected, but not      
/// implemented in some particular way.                                       
#define ignore_all_intents(FOR_TYPE) \
   template<template<class> class I> requires ::Langulus::CT::Intent<I<FOR_TYPE>> \
   explicit constexpr FOR_TYPE(I<FOR_TYPE>&& meta) noexcept \
      : FOR_TYPE {*meta} {} \
   template<template<class> class I> requires ::Langulus::CT::Intent<I<FOR_TYPE>> \
   constexpr FOR_TYPE& operator = (I<FOR_TYPE>&& rhs) noexcept { \
      new (this) FOR_TYPE {*rhs}; \
      return *this; \
   }


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
      static_assert(CT::Complete<T>,
         "T has to be complete in order to be constructed");
      static_assert(not CT::Abstract<T>,
         "T has to be concrete in order to be constructed");
      static_assert(not CT::Reference<T>,
         "T can't be a reference in order to be constructed");

      AssumeDev(placement, HERE(), "Invalid placement pointer");

      if constexpr (CT::Referred<S<T>>) {
         // Refer                                                       
         if constexpr (CT::HasReferConstructor<T>)
            return new (placement) T {FWD(value)};
         else if constexpr (::std::copy_constructible<T>)
            return new (placement) T {*value};
         else {
            static_assert(FAKE, "Can't refer-construct type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Moved<S<T>>) {
         // Move                                                        
         if constexpr (CT::HasMoveConstructor<T>)
            return new (placement) T {FWD(value)};
         else if constexpr (::std::move_constructible<T>)
            return new (placement) T {*value};
         else {
            static_assert(FAKE, "Can't move-construct type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Abandoned<S<T>>) {
         // Abandon                                                     
         if constexpr (CT::HasAbandonConstructor<T>)
            return new (placement) T {FWD(value)};
         else if constexpr (CT::HasMoveConstructor<T>)
            return new (placement) T {Move(*value)};
         else if constexpr (::std::move_constructible<T>)
            return new (placement) T {*value};
         else {
            static_assert(FAKE,
               "Can't abandon-construct destructible type"
               " - explicit abandon-constructor is required");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Cloned<S<T>>) {
         // Clone                                                       
         // @attention - assumes that all levels of indirection have    
         //    been allocated and pointers point to valid memory        
         using DT = Decay<T>;
         static_assert(CT::Complete<DT>,
            "Can't clone-construct an incomplete type");

         if constexpr (CT::NotVoid<DT>) {
            if constexpr (CT::HasCloneConstructor<DT>)
               return new (placement) DT {Clone(DenseCast(*value))};
            else if constexpr (CT::POD<DT> and CT::HasReferConstructor<DT>)
               return new (placement) DT {Refer(DenseCast(*value))};
            else if constexpr (CT::POD<DT> and ::std::copy_constructible<DT>)
               return new (placement) DT {DenseCast(*value)};
            else {
               static_assert(FAKE, "Can't clone-construct type");
               return Unsupported {};
            }
         }
         else {
            static_assert(FAKE, "Can't clone-construct a void type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Copied<S<T>>) {
         // Copy                                                        
         if constexpr (CT::HasCopyConstructor<T>)
            return new (placement) T {FWD(value)};
         else if constexpr (CT::POD<T> and CT::HasReferConstructor<T>)
            return new (placement) T {Refer(*value)};
         else if constexpr (CT::POD<T> and ::std::copy_constructible<T>)
            return new (placement) T {*value};
         else {
            static_assert(FAKE, "Can't copy-construct type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Disowned<S<T>>) {
         // Disown                                                      
         if constexpr (CT::HasDisownConstructor<T>)
            return new (placement) T {FWD(value)};
         else if constexpr (CT::POD<T> and CT::HasReferConstructor<T>)
            return new (placement) T {Refer(*value)};
         else if constexpr (CT::POD<T> and ::std::copy_constructible<T>)
            return new (placement) T {*value};
         else {
            static_assert(FAKE, "Can't disown-construct type");
            return Unsupported {};
         }
      }
      else static_assert(false, "Intent wasn't recognized");
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
      static_assert(CT::Mutable<T>,
         "T has to be mutable in order to be assigned");
      static_assert(CT::Complete<T>,
         "T has to be complete in order to be assigned");
      static_assert(not CT::Reference<T>,
         "T can't be a reference in order to be assigned");

      if constexpr (CT::Referred<S<T>>) {
         // Refer                                                       
         if constexpr (CT::HasReferAssign<T>)
            return (lhs = FWD(rhs));
         else if constexpr (::std::is_copy_assignable_v<T>)
            return (lhs = *rhs);
         else {
            static_assert(FAKE, "Can't refer-assign type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Moved<S<T>>) {
         // Move                                                        
         if constexpr (CT::HasMoveAssign<T>)
            return (lhs = FWD(rhs));
         else if constexpr (::std::is_move_assignable_v<T>)
            return (lhs = *rhs);
         else {
            static_assert(FAKE, "Can't move-assign type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Abandoned<S<T>>) {
         // Abandon                                                     
         if constexpr (CT::HasAbandonAssign<T>)
            return (lhs = FWD(rhs));
         else if constexpr (CT::HasMoveAssign<T>)
            return (lhs = Move(*rhs));
         else if constexpr (::std::is_move_assignable_v<T>)
            return (lhs = *rhs);
         else {
            static_assert(FAKE,
               "Can't abandon-assign destructible type"
               " - explicit abandon-assigner is required");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Cloned<S<T>>) {
         // Clone                                                       
         // @attention - assumes that all levels of indirection have    
         //    been allocated and pointers point to valid memory        
         using DT = Decay<T>;
         static_assert(CT::Complete<DT>,
            "Can't clone-assign an incomplete type");

         if constexpr (CT::NotVoid<DT>) {
            if constexpr (CT::Mutable<Deptr<T>>) {
               if constexpr (CT::HasCloneAssign<DT>)
                  return (DenseCast(lhs) = Clone(DenseCast(*rhs)));
               else if constexpr (CT::POD<DT> and CT::HasReferAssign<DT>)
                  return (DenseCast(lhs) = Refer(DenseCast(*rhs)));
               else if constexpr (CT::POD<DT> and ::std::is_copy_assignable_v<DT>)
                  return (DenseCast(lhs) = DenseCast(*rhs));
               else {
                  static_assert(FAKE, "Can't clone-assign type");
                  return Unsupported {};
               }
            }
            else {
               static_assert(FAKE, "Can't clone-assign type - lhs is not mutable");
               return Unsupported {};
            }
         }
         else {
            static_assert(FAKE, "Can't clone-assign void or incomplete type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Copied<S<T>>) {
         // Copy                                                        
         if constexpr (CT::HasCopyAssign<T>)
            return (lhs = FWD(rhs));
         else if constexpr (CT::POD<T> and CT::HasReferAssign<T>)
            return (lhs = Refer(*rhs));
         else if constexpr (CT::POD<T> and ::std::is_copy_assignable_v<T>)
            return (lhs = *rhs);
         else {
            static_assert(FAKE, "Can't copy-assign type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Disowned<S<T>>) {
         // Disown                                                      
         if constexpr (CT::HasDisownAssign<T>)
            return (lhs = FWD(rhs));
         else if constexpr (CT::POD<T> and CT::HasReferAssign<T>)
            return (lhs = Refer(*rhs));
         else if constexpr (CT::POD<T> and ::std::is_copy_assignable_v<T>)
            return (lhs = *rhs);
         else {
            static_assert(FAKE, "Can't disown-assign type");
            return Unsupported {};
         }
      }
      else static_assert(false, "Intent wasn't recognized");
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
      /// C++20 semantics. Constructors are remarkably consistent across      
      /// compilers. Unlike assignments, that is (see below)...               
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
      concept DisownConstructible = Inner::CheckSize<T...>()
          and (IntentConstructible<Langulus::Disown, T> and ...);

      /// Check if all Decay<T> are clone-constructible                       
      /// Does a deep copy. If POD, Decay<T> can be clone-constructible even  
      /// if not having the specific clone constructor, as long as T is       
      /// std::copy_constuctible                                              
      template<class...T>
      concept CloneConstructible = Inner::CheckSize<T...>()
          and (IntentConstructible<Langulus::Clone, T> and ...);

      /// Check if all T are abandon-constructible                            
      /// Does a move but doesn't fully reset source as an optimization -     
      /// assuming that the abandoned instance is never going to be used in   
      /// more ways, than just calling the destructor. T can be               
      /// abandon-constructible even if not having the specific abandon       
      /// constructor, as long as it is std::move_constuctible                
      template<class...T>
      concept AbandonConstructible = Inner::CheckSize<T...>()
          and (IntentConstructible<Langulus::Abandon, T> and ...);

      /// Check if all T are refer-constructible                              
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership. T can be refer-constructible as long as T is             
      /// std::copy_constuctible                                              
      template<class...T>
      concept ReferConstructible = Inner::CheckSize<T...>()
          and (IntentConstructible<Langulus::Refer, T> and ...);
      
      /// Check if all T are copy-constructible                               
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      /// If POD, T can be copy-constructible even if not having the specific 
      /// shallow-copy constructor, as long as T is std::copy_constuctible    
      template<class...T>
      concept CopyConstructible = Inner::CheckSize<T...>()
          and (IntentConstructible<Langulus::Copy, T> and ...);

      /// Check if all T are move-constructible                               
      /// Does a move, fully resetting source into a reusable state           
      /// T is move-constructible as long as it is std::move_constuctible     
      template<class...T>
      concept MoveConstructible = Inner::CheckSize<T...>()
          and (IntentConstructible<Langulus::Move, T> and ...);


      ///                                                                     
      ///   Assignables                                                       
      ///                                                                     
      ///   These concepts are bit looser on requirements, compared to their  
      /// Has*Assign counterparts, to allow for fallbacks in places where     
      /// they are required. A type may not explicitly HasAbandonAssign,      
      /// and yet be AbandonAssignable, because it is movable by the usual    
      /// C++20 semantics.                                                    
      /// @attention these hit a lot of compiler bugs on different compilers: 
      /// - it causes ambiguity on Clang 19.1 for refer intents, because      
      ///   the compiler can't decide whether to implicit-cast to && or       
      ///   const&. I've added explicit intent assigners to compensate for    
      ///   that                                                              
      /// - it causes ambiguity on GCC 14.2 for move/abandon intents, because 
      ///   the compiler can't decide how to implicit-cast to && or           
      ///   const&. I've added explicit intent assigners to compensate for    
      ///   that                                                              
      /// - there is also this nasty compiler bug on MSVC v143 that affects   
      ///   types is deleted destructors, and implicit copy/move semantics    
      ///   https://stackoverflow.com/questions/79665049                      
      ///                                                                     
      /// @note these compiler defects affect only CT::HasReferAssign and     
      ///    CT::HasMoveAssign/CT::HasAbandonAssign. On the other hand,       
      ///    CT::ReferAssignable and CT::MoveAssignable/CT::AbandonAssignable 
      ///    remain unaffected, so if you want consistent behavior across     
      ///    compilers, just use the IntentAssign function instead of '='     
      ///                                                                     
      /// In that sense, none of these concepts here guarantees, that an      
      /// adequate intent-assignment exists for a type, unless you use        
      /// IntentAssign itself                                                 
      ///                                                                     

      /// Check if all T are intent-assignable by intent S                    
      /// T can be intent-assignable even if not having an explicit assigner, 
      /// as long as T and S are compatible with the usual C++20 semantics    
      ///   @tparam S - the intent                                            
      ///   @tparam T... - the types                                          
      template<template<class> class S, class...T>
      concept IntentAssignable = NotVoid<T...> and Mutable<T...> and Intent<S<Decvq<T>>...>
          and (requires (Decvq<T> t, S<Decvq<T>>&& a) {
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
      concept DisownAssignable = Inner::CheckSize<T...>()
          and (IntentAssignable<Langulus::Disown, T> and ...);

      /// Check if all Decay<T> are clone-assignable                          
      /// Does a deep copy                                                    
      /// If POD, Decay<T> can be clone-assignable even if not having an      
      /// explicit clone-assignment, as long as std::copy_assignable<T> holds 
      template<class...T>
      concept CloneAssignable = Inner::CheckSize<T...>()
          and (IntentAssignable<Langulus::Clone, T> and ...);

      /// Check if all T are abandon-assignable                               
      /// Does a move, but doesn't fully reset source (optimization)          
      /// T can be abandon-assignable even if not having an explicit          
      /// abandon-assignment, as long as std::move_assignable<T> holds        
      template<class...T>
      concept AbandonAssignable = Inner::CheckSize<T...>()
          and (IntentAssignable<Langulus::Abandon, T> and ...);

      /// Check if all T are refer-assignable                                 
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership.                                                          
      /// T can be refer-assignable as long as std::copy_assignable<T> holds  
      template<class...T>
      concept ReferAssignable = Inner::CheckSize<T...>()
          and (IntentAssignable<Langulus::Refer, T> and ...);
      
      /// Check if all T are copy-assignable                                  
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      /// If POD, T can be copy-assignable even if not having an explicit     
      /// copy-assigner, as long as std::copy_assignable<T> holds             
      template<class...T>
      concept CopyAssignable = Inner::CheckSize<T...>()
          and (IntentAssignable<Langulus::Copy, T> and ...);

      /// Check if all T are move-assignable                                  
      /// Does a move, fully resetting source                                 
      /// T is move-assignable as long as std::move_assignable<T> holds       
      /// @attention you can't have move semantics, if a type has its         
      ///   destructor deleted - every time you move an instance, the old one 
      ///   has to be deleted later.                                          
      template<class...T>
      concept MoveAssignable = Inner::CheckSize<T...>()
          and (IntentAssignable<Langulus::Move, T> and ...);

   } // namespace Langulus::CT

} // namespace Langulus
