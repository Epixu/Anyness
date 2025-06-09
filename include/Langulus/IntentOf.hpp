///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "TypeOf.hpp"
#include "CT/Derived.hpp"
#include "CT/POD.hpp"


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
   ///   unsigned Depth - decides whether the semantic is deep or shallow     
   ///   bool     Keep  - decides whether to exercise ownership or not        
   ///   bool     Move  - decides whether it's a move semantic or not         

   /// Checks if all T are shallow intents                                    
   /// Shallow intents are propagated through mostly a single indirection     
   template<class...T>
   concept ShallowIntent = ((Intent<Deref<T>> and Decay<T>::IsShallow()) and ...);

   /// Checks if all T are deep intents                                       
   /// Deep intents propagate through all levels of indirection               
   template<class...T>
   concept DeepIntent = ((Intent<Deref<T>> and not Decay<T>::IsShallow()) and ...);

   /// Check if all T are refer intents                                       
   /// Does a shallow-copy without delving into any indirections, while       
   /// exercising ownership of managed data                                   
   template<class...T>
   concept Referred = ((Intent<Deref<T>> and Decay<T>::Is(0, true, false)) and ...);
      
   /// Check if all T are copy intents                                        
   /// Does a shallow-copy, while cloning only the first indirection level    
   template<class...T>
   concept Copied = ((Intent<Deref<T>> and Decay<T>::Is(1, true, false)) and ...);

   /// Check if all T are move intents                                        
   /// Moves by leaving the moved instances reusable                          
   template<class...T>
   concept Moved = ((Intent<Deref<T>> and Decay<T>::Is(0, true, true)) and ...);

   /// Check if all T are abandon intents                                     
   /// Moves by leaving the moved instances no longer usable                  
   template<class...T>
   concept Abandoned = ((Intent<Deref<T>> and Decay<T>::Is(0, false, true)) and ...);

   /// Check if all T are disown intents                                      
   /// Does a shallow-copy without delving into any indirections, without     
   /// exercising any ownership                                               
   template<class...T>
   concept Disowned = ((Intent<Deref<T>> and Decay<T>::Is(0, false, false)) and ...);

   /// Check if all T are clone intents                                       
   /// Does a deep-copy throughout all levels of indirection                  
   template<class...T>
   concept Cloned = ((DeepIntent<Deref<T>> and Decay<T>::Is(true, false)) and ...);

} // namespace Langulus::CT

namespace Langulus
{
   
   /// Shed only the intent from a type, if any                               
   template<class T>
   using Deint = Tif<CT::Intent<Deref<T>>, TypeOf<T>, T>;

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

      /// Implicitly collapse the intent                                      
      /// This way the wrapper is seamlessly integrated with the standard     
      /// C++20 copy semantics                                                
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () const noexcept { return mValue; }
   };

   template<CT::NoIntent T>
   Refer(const T&) -> Refer<T>;

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

      /// Implicitly collapse the intent, but only when applying it to        
      /// POD/Sparse, since Refer is isomorphic to Copy in those cases        
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () const noexcept
      requires (CT::POD<T> or CT::Sparse<T>) {
         return mValue;
      }

      /// Otherwise the collapse can only be explicit                         
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator const T& () const noexcept
      requires (not CT::POD<T> and not CT::Sparse<T>) {
         return mValue;
      }
   };

   template<CT::NoIntent T>
   Copy(const T&) -> Copy<T>;
   
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

      /// Implicitly collapse the intent                                      
      /// This way the wrapper is seamlessly integrated with the standard     
      /// C++20 move semantics                                                
      LANGULUS(ALWAYS_INLINED)
      constexpr operator T&& () const noexcept { return FWD(mValue); }
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

      /// Implicitly collapse the intent                                      
      /// This way the wrapper is seamlessly integrated with the standard     
      /// C++20 move semantics                                                
      LANGULUS(ALWAYS_INLINED)
      constexpr operator T&& () const noexcept { return FWD(mValue); }
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

      /// Implicitly collapse the intent, but only when applying it to PODs,  
      /// since they are never allowed to have ownership either way           
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () const noexcept requires CT::POD<T> {
         return mValue;
      }

      /// Otherwise the collapse can only be explicit                         
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator const T& () const noexcept requires CT::NotPOD<T> {
         return mValue;
      }
   };
   
   template<CT::NoIntent T>
   Disown(const T&) -> Disown<T>;

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

      /// Implicitly collapse the intent, when applying it to PODs,           
      /// since they are always cloned upon copy (BUT ONLY IF `T` IS DENSE)   
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () const noexcept
      requires (CT::POD<T> and CT::Dense<T>) {
         return mValue;
      }

      /// Otherwise the collapse can only be explicit                         
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator const T& () const noexcept
      requires (CT::NotPOD<T> or CT::Sparse<T>) {
         return mValue;
      }
   };
   
   template<CT::NoIntent T>
   Clone(const T&) -> Clone<T>;

   template<CT::Intent T>
   Clone(T&&) -> Clone<Decq<Deref<TypeOf<T>>>>;


   namespace CT
   {

      ///                                                                     
      ///   Intent type traits                                                
      ///                                                                     
      ///   These concepts are strict requirements, and are true only if the  
      /// corresponding constructors/assigners are implicitly/explicitly      
      /// defined. No fallbacks!                                              
      ///                                                                     

      /// Check if all T have intent constructors for S                       
      ///   @tparam S - the intent                                            
      ///   @tparam T... - the types                                          
      template<template<class> class S, class...T>
      concept HasIntentConstructor = Intent<S<T>...>
          and requires (S<T>&&...a) { (T (FWD(a)), ...); };

      /// Check if all TypeOf<S> have intent constructors for S               
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept HasIntentConstructorAlt = Intent<Deref<S>...>
          and requires (S&&...a) { (TypeOf<S> (FWD(a)), ...); };

      /// Check if all T have a disown-constructor                            
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      template<class...T>
      concept HasDisownConstructor = (HasIntentConstructor<::Langulus::Disown, T> and ...);

      /// Check if all Decay<T> have a clone-constructor                      
      /// Does a deep copy                                                    
      template<class...T>
      concept HasCloneConstructor = (HasIntentConstructor<::Langulus::Clone, T> and ...);

      /// Check if all T have a abandon-constructor                           
      /// Does a move, but doesn't fully reset source (optimization)          
      template<class...T>
      concept HasAbandonConstructor = (HasIntentConstructor<::Langulus::Abandon, T> and ...);

      /// Check if all T have a refer-constructor                             
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership.                                                          
      /// T has refer-constructor as long as it is std::copy_constuctible     
      template<class...T>
      concept HasReferConstructor = ((HasIntentConstructor<::Langulus::Refer, T>
           or ::std::copy_constructible<T>) and ...);
      
      /// Check if all T have a copy-constructor (don't mistake it for a      
      /// std::copy_constructible!)                                           
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      template<class...T>
      concept HasCopyConstructor = (HasIntentConstructor<::Langulus::Copy, T> and ...);

      /// Check if all T have a move-constructor                              
      /// Does a move, fully resetting source                                 
      /// T has move-constructor as long as it is std::move_constuctible      
      template<class...T>
      concept HasMoveConstructor = ((Sparse<T>
           or HasIntentConstructor<::Langulus::Move, T>
           or ::std::move_constructible<T>) and ...);

      /// Check if all T have an intent-assigner for S                        
      ///   @tparam S - the intent                                            
      ///   @tparam T... - the types                                          
      template<template<class> class S, class...T>
      concept HasIntentAssign = ((Intent<S<T>>
          and ::std::assignable_from<T&, S<T>&&>) and ...);

      /// Check if all TypeOf<S> has intent-assigner for S                    
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept HasIntentAssignAlt = ((Intent<Deref<S>>
          and ::std::assignable_from<TypeOf<S>&, S&&>) and ...);

      /// Check if all T have a disown-assigner                               
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      template<class...T>
      concept HasDisownAssign = (HasIntentAssign<::Langulus::Disown, T> and ...);

      /// Check if all Decay<T> have a clone-assigner                         
      /// Does a deep copy                                                    
      template<class...T>
      concept HasCloneAssign = (HasIntentAssign<::Langulus::Clone, T> and ...);

      /// Check if all T have an abandon-assigner                             
      /// Does a move, but doesn't fully reset source (optimization)          
      template<class...T>
      concept HasAbandonAssign = (HasIntentAssign<::Langulus::Abandon, T> and ...);

      /// Check if all T have refer-assigner                                  
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership.                                                          
      /// T has a refer-assigner as long as std::copy_assignable<T> holds     
      template<class...T>
      concept HasReferAssign = ((HasIntentAssign<::Langulus::Refer, T>
           or ::std::assignable_from<T&, const T&>) and ...);
      
      /// Check if all T have a copy-assigner (don't mistake it for a         
      /// std::copy_assignable!)                                              
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      template<class...T>
      concept HasCopyAssign = (HasIntentAssign<::Langulus::Copy, T> and ...);

      /// Check if all T have a move-assigner                                 
      /// Does a move, fully resetting source                                 
      /// T has a move-assigner as long as std::assignable_from<T&, T&&> holds
      /// This includes the cases when the type has a default copy-assign to  
      /// which the compiler falls back to. In that case move-assignment is   
      /// the same as refer-assignment.                                       
      template<class...T>
      concept HasMoveAssign = ((HasIntentAssign<::Langulus::Move, T>
           or ::std::assignable_from<T&, T&&>) and ...);

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
