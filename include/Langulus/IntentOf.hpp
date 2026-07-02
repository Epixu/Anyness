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


namespace Langulus::CTTI
{
   /// Affects CT::Intent                                                     
   template<class T>
   struct Intent;
}

LANGULUS_CTTI_CONCEPT_UNSHEDDABLE_DECVQ(Intent);

/// MARK: Concepts                                                            
namespace Langulus::CT
{
   /// Check if all T are NOT sheddable intents                               
   template<class...T>
   concept NoIntent = NotIntent<T...>;


   ///                                                                        
   /// All intents are defined in terms of three properties, and the          
   /// combinations between them:                                             
   ///   uint Depth - decides whether the intent is deep or shallow           
   ///   bool Keep  - decides whether to exercise ownership or not            
   ///   bool Move  - decides whether it's a move semantic or not             

   /// Checks if all T are shallow intents                                    
   /// Shallow intents are propagated through mostly a single indirection     
   template<class...T>
   concept ShallowIntent = Validate<T...> and ((
         Intent<Decvq<Deref<T>>> and Decvq<Deref<T>>::IsShallow()
      ) and ...);

   /// Checks if all T are deep intents                                       
   /// Deep intents propagate through all levels of indirection               
   template<class...T>
   concept DeepIntent = Validate<T...> and ((
         Intent<Decvq<Deref<T>>> and not Decvq<Deref<T>>::IsShallow()
      ) and ...);

   /// Check if all T are refer intents                                       
   /// Does a shallow-copy without delving into any indirections, while       
   /// exercising ownership of managed data                                   
   template<class...T>
   concept Referred = Validate<T...> and ((
         Intent<Decvq<Deref<T>>> and Decvq<Deref<T>>::Is(0, true, false)
      ) and ...);
      
   /// Check if all T are copy intents                                        
   /// Does a shallow-copy, while cloning only the first indirection level    
   template<class...T>
   concept Copied = Validate<T...> and ((
         Intent<Decvq<Deref<T>>> and Decvq<Deref<T>>::Is(1, true, false)
      ) and ...);

   /// Check if all T are move intents                                        
   /// Moves by leaving the moved instances reusable                          
   template<class...T>
   concept Moved = Validate<T...> and ((
         Intent<Decvq<Deref<T>>> and Decvq<Deref<T>>::Is(0, true, true)
      ) and ...);

   /// Check if all T are abandon intents                                     
   /// Moves by leaving the moved instances no longer usable                  
   template<class...T>
   concept Abandoned = Validate<T...> and ((
         Intent<Decvq<Deref<T>>> and Decvq<Deref<T>>::Is(0, false, true)
      ) and ...);

   /// Check if all T are disown intents                                      
   /// Does a shallow-copy without delving into any indirections, without     
   /// exercising any ownership                                               
   template<class...T>
   concept Disowned = Validate<T...> and ((
         Intent<Decvq<Deref<T>>> and Decvq<Deref<T>>::Is(0, false, false)
      ) and ...);

   /// Check if all T are clone intents                                       
   /// Does a deep-copy throughout all levels of indirection                  
   template<class...T>
   concept Cloned = Validate<T...> and ((
         DeepIntent<Decvq<Deref<T>>> and Decvq<Deref<T>>::Is(true, false)
      ) and ...);
}

namespace Langulus
{
   namespace Inner
   {
      template<class T>
      consteval auto GetDeintInner() {
         if constexpr (CT::Intent<T>)
            return Types<TypeOf<T>> {};
         else
            return Types<T> {};
      }
   }

   /// Shed only the intent from a type, if any                               
   template<class T>
   using Deint = typename decltype(Inner::GetDeintInner<T>())::First;

   /// Decay an intent to the contained data                                  
   ///   @param intent the intent to decay                                    
   ///   @return the forwarded inner data                                     
   template<class T> LANGULUS(ALWAYS_INLINED)
   constexpr decltype(auto) DeintCast(T&& intent) noexcept {
      if constexpr (CT::Intent<T>) return LglsFwd(intent.what);
      else                         return LglsFwd(intent);
   }

   namespace Inner
   {
      /// Helper base that defines intent properties                          
      ///   @tparam DEPTH the depth of the intent, use -1 for infinite        
      ///   @tparam KEEP does the intent practice ownership                   
      ///   @tparam MOVE does the intent involve transfer of ownership        
      template<uint DEPTH, bool KEEP, bool MOVE>
      struct CommonIntent {
         using CTTI_ReflectAs     = void;
         using CTTI_Abstract      = Yes<>;
         using CTTI_Allocatable   = No;
         using CTTI_Intent        = Yes<>;

         static consteval uint GetDepth()     { return DEPTH; }
         static consteval bool IsKept()       { return KEEP;  }
         static consteval bool IsMoved()      { return MOVE;  }
         static consteval bool ResetsOnMove() { return KEEP and MOVE; }
         static consteval bool KeepsOnCopy()  { return KEEP and not MOVE; }
         static consteval bool IsShallow()    { return DEPTH < 2; }
         static consteval bool Is(int depth, bool keep, bool move) {
            return DEPTH == depth and KEEP == keep and MOVE == move;
         }
         static consteval bool Is(bool keep, bool move) {
            return KEEP == keep and MOVE == move;
         }
      };
   }


   /// MARK: Refer                                                            
   ///                                                                        
   /// Referred value intermediate type, used in constructors and assignments 
   /// to refer to data explicitly                                            
   ///   @tparam T the type to refer                                          
   template<class T> requires (not ::std::is_reference_v<T>)
   struct Refer final : Inner::CommonIntent<0, true, false> {
      const T& what;

      using CTTI_Typed     = decltype(what);
      using CTTI_Sheddable = decltype(what);

      template<class ALT>
      using Retype = Refer<Decq<Deref<Deint<ALT>>>>;

      Refer() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Refer(Decvq<T>& value) noexcept : what {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Refer(const T& value) noexcept : what {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Refer(CT::Intent auto&& value) noexcept : what {value.what} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      /// Forward as referred                                                 
      ///   @tparam ALT_T optional type to forward as                         
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
            return               static_cast<const ALT_T&>(what);
         else
            return Refer<ALT_T> (static_cast<const ALT_T&>(what));
      }

      /// Refer something else                                                
      ///   @param value the value to refer (can be an intent)                
      ///   @return the referred value, disregarding previous intent          
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast(value);
         else
            return Retype<ALT_T> (LglsFwd(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T* operator -> () const noexcept { return SparseCast(what); }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator bool () const noexcept
      requires requires { static_cast<bool>(what); } {
         return static_cast<bool>(what);
      }
   };

   template<CT::NoIntent T>
   Refer(T&) -> Refer<T>;

   template<CT::Intent T>
   Refer(T&&) -> Refer<Decq<Deref<TypeOf<T>>>>;

   template<CT::Intent T>
   Refer(T const&) -> Refer<Decq<Deref<TypeOf<T>>>>;

   
   /// MARK: Copy                                                             
   ///                                                                        
   /// Copied value intermediate type, used in constructors and assignments   
   /// to shallow-copy container explicitly                                   
   ///   @tparam T the type to copy                                           
   template<class T> requires (not ::std::is_reference_v<T>)
   struct Copy final : Inner::CommonIntent<1, true, false> {
      const T& what;
      
      using CTTI_Typed     = decltype(what);
      using CTTI_Sheddable = decltype(what);

      template<class ALT>
      using Retype = Copy<Decq<Deref<Deint<ALT>>>>;

      Copy() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Copy(const T& value) noexcept : what {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Copy(CT::Intent auto&& value) noexcept : what {value.what} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      /// Forward as copied                                                   
      ///   @tparam ALT_T optional type to forward as                         
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
            return static_cast<const ALT_T&>(what);
         else
            return Copy<ALT_T> {what};
      }

      /// Copy something else                                                 
      ///   @param value the value to copy (can be an intent)                 
      ///   @return the copied value, disregarding previous intent            
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast(value);
         else
            return Retype<ALT_T> (LglsFwd(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T* operator -> () const noexcept { return SparseCast(what); }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator bool () const noexcept
      requires requires { static_cast<bool>(what); } {
         return static_cast<bool>(what);
      }
   };

   template<CT::NoIntent T>
   Copy(T&) -> Copy<T>;
   
   template<CT::Intent T>
   Copy(T&&) -> Copy<Decq<Deref<TypeOf<T>>>>;

   template<CT::Intent T>
   Copy(T const&) -> Copy<Decq<Deref<TypeOf<T>>>>;


   /// MARK: Move                                                             
   ///                                                                        
   /// Moved value intermediate type, used in constructors and assignments    
   /// to move data explicitly                                                
   ///   @tparam T the type to move                                           
   template<class T> requires (not ::std::is_reference_v<T>)
   struct Move final : Inner::CommonIntent<0, true, true> {
      static_assert(CT::Mutable<T>, "Constant T isn't movable");
      T&& what;

      using CTTI_Typed     = decltype(what);
      using CTTI_Sheddable = decltype(what);

      template<class ALT>
      using Retype = Move<Decq<Deref<Deint<ALT>>>>;

      Move() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Move(T& value) noexcept : what {LglsMov(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Move(T&& value) noexcept : what {LglsFwd(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      constexpr Move(Move const& value) noexcept : what {LglsFwd(value.what)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Move(CT::Intent auto&& value) noexcept : what {LglsFwd(value.what)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      /// Forward as moved                                                    
      ///   @tparam ALT_T optional type to forward as                         
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
            return              static_cast<ALT_T&&>(what);
         else
            return Move<ALT_T> {static_cast<ALT_T&&>(what)};
      }

      /// Move something else                                                 
      ///   @param value the value to move (can be an intent)                 
      ///   @return the moved value, disregarding previous intent             
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast     (LglsFwd(value));
         else
            return Retype<ALT_T> (LglsFwd(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr T* operator -> () const noexcept { return SparseCast(what); }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator bool () const noexcept
      requires requires { static_cast<bool>(what); } {
         return static_cast<bool>(what);
      }
   };

   template<CT::NoIntent T>
   Move(T&&) -> Move<Deref<T>>;

   template<CT::Intent T>
   Move(T&&) -> Move<Decq<Deref<TypeOf<T>>>>;


   /// MARK: Abandon                                                          
   ///                                                                        
   /// Abandoned value intermediate type can be used in constructors and      
   /// assignments to provide a guarantee, that the value shall not be used   
   /// after being consumed, so we can save up on resetting it fully.         
   /// For example, you can construct a Many with an abandoned Many, which is 
   /// same as move-construction, but the abandoned Many shall have only its  
   /// allocation reset, instead of the entire container, leaving it in a     
   /// state that is unfit for reuse, saving a lot of instructions.           
   ///   @tparam T the type to abandon                                        
   template<class T> requires (not ::std::is_reference_v<T>)
   struct Abandon final : Inner::CommonIntent<0, false, true> {
      static_assert(CT::Mutable<T>, "Constant T isn't abandonable");
      T&& what;

      using CTTI_Typed     = decltype(what);
      using CTTI_Sheddable = decltype(what);

      template<class ALT>
      using Retype = Abandon<Decq<Deref<Deint<ALT>>>>;

      Abandon() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Abandon(T& value) noexcept : what {LglsMov(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Abandon(T&& value) noexcept : what {LglsFwd(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr Abandon(Abandon const& value) noexcept : what{LglsFwd(value.what)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Abandon(CT::Intent auto&& value) noexcept : what {LglsFwd(value.what)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      /// Forward as abandoned                                                
      ///   @tparam ALT_T optional type to forward as                         
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
            return                 static_cast<ALT_T&&>(what);
         else
            return Abandon<ALT_T> {static_cast<ALT_T&&>(what)};
      }

      /// Abandon something else                                              
      ///   @param value the value to abandon (can be an intent)              
      ///   @return the abandoned value, disregarding previous intent         
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast     (LglsFwd(value));
         else
            return Retype<ALT_T> (LglsFwd(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr T* operator -> () const noexcept { return SparseCast(what); }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator bool () const noexcept
      requires requires { static_cast<bool>(what); } {
         return static_cast<bool>(what);
      }
   };
   
   template<CT::NoIntent T>
   Abandon(T&&) -> Abandon<Deref<T>>;

   template<CT::Intent T>
   Abandon(T&&) -> Abandon<Decq<Deref<TypeOf<T>>>>;


   /// MARK: Disown                                                           
   ///                                                                        
   /// Disowned value intermediate type, used in constructors and assignments 
   /// to copy container without gaining ownership                            
   ///   @tparam T the type to disown                                         
   template<class T> requires (not ::std::is_reference_v<T>)
   struct Disown final : Inner::CommonIntent<0, false, false> {
      const T& what;

      using CTTI_Typed     = decltype(what);
      using CTTI_Sheddable = decltype(what);

      template<class ALT>
      using Retype = Disown<Decq<Deref<Deint<ALT>>>>;

      Disown() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Disown(const T& value) noexcept : what {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Disown(CT::Intent auto&& value) noexcept : what {value.what} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      /// Forward as disowned                                                 
      ///   @tparam ALT_T optional type to forward as                         
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
            return static_cast<const ALT_T&>(what);
         else
            return Disown<ALT_T> {static_cast<const ALT_T&>(what)};
      }

      /// Disown something else                                               
      ///   @param value the value to disown (can be an intent)               
      ///   @return the disowned value, disregarding previous intent          
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Aggregate<ALT_T>)
            return DeintCast(value);
         else
            return Retype<ALT_T> (LglsFwd(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T* operator -> () const noexcept { return SparseCast(what); }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator bool () const noexcept
      requires requires { static_cast<bool>(what); } {
         return static_cast<bool>(what);
      }
   };
   
   template<CT::NoIntent T>
   Disown(T&) -> Disown<T>;

   template<CT::Intent T>
   Disown(T&&) -> Disown<Decq<Deref<TypeOf<T>>>>;

   template<CT::Intent T>
   Disown(T const&) -> Disown<Decq<Deref<TypeOf<T>>>>;


   /// MARK: Clone                                                            
   ///                                                                        
   /// Cloned value intermediate type, used in constructors and assignments   
   /// to clone container, doing a deep copy instead of default shallow one   
   ///   @tparam T the type to clone                                          
   template<class T> requires (not ::std::is_reference_v<T>)
   struct Clone final : Inner::CommonIntent<static_cast<uint>(-1), true, false> {
      const T& what;
      
      using CTTI_Typed     = decltype(what);
      using CTTI_Sheddable = decltype(what);

      template<class ALT>
      using Retype = Clone<Decq<Deref<Deint<ALT>>>>;

      Clone() = delete;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Clone(const T& value) noexcept : what {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Clone(CT::Intent auto&& value) noexcept : what {value.what} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      /// Forward as cloned, never collapse                                   
      template<class ALT_T = T> LANGULUS(ALWAYS_INLINED)
      constexpr decltype(auto) Forward() const noexcept {
         static_assert(CT::NoIntent<ALT_T>,
            "Can't nest intents");
         static_assert(CT::DerivedFrom<T, ALT_T>,
            "Can't forward as this type");
         return Clone<ALT_T> {what};
      }

      /// Clone something else                                                
      template<class ALT_T> LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(ALT_T&& value) noexcept {
         return Retype<ALT_T> (LglsFwd(value));
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr const T* operator -> () const noexcept { return SparseCast(what); }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr operator bool () const noexcept
      requires requires { static_cast<bool>(what); } {
         return static_cast<bool>(what);
      }
   };
   
   template<CT::NoIntent T>
   Clone(T&) -> Clone<T>;

   template<CT::Intent T>
   Clone(T&&) -> Clone<Decq<Deref<TypeOf<T>>>>;

   template<CT::Intent T>
   Clone(T const&) -> Clone<Decq<Deref<TypeOf<T>>>>;
   

   /// MARK: CT::Has*                                                         
   namespace CT
   {
      ///                                                                     
      ///   Intent type traits                                                
      ///                                                                     
      /// These concepts are strict requirements, and are true only if the    
      /// corresponding constructors/assigners are defined. No fallbacks!     
      ///                                                                     

      /// Check if all T have dedicated intent constructors for S             
      ///   @tparam S the intent                                              
      ///   @tparam T the types                                               
      template<template<class> class S, class...T>
      concept HasIntentConstructor = Intent<S<T>...> and not Aggregate<T...>
          and requires (S<T>&&...arg) { (T {LglsFwd(arg)}, ...); };

      /// Check if all TypeOf<S> have a dedicated intent constructor for S    
      ///   @tparam S the intents and types                                   
      template<class...S>
      concept HasIntentConstructorAlt = Intent<S...> and not Aggregate<TypeOf<S>...>
          and requires (S&&...arg) { (Decvq<Deref<TypeOf<S>>> {LglsFwd(arg)}, ...); };

      /// Check if all T have a dedicated disown-constructor                  
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      template<class...T>
      concept HasDisownConstructor = Validate<T...>
          and (HasIntentConstructor<::Langulus::Disown, T> and ...);

      /// Check if all Decay<T> have a dedicated clone-constructor            
      /// Does a deep copy                                                    
      template<class...T>
      concept HasCloneConstructor = Validate<T...>
          and (HasIntentConstructor<::Langulus::Clone, T> and ...);

      /// Check if all T have a dedicated abandon-constructor                 
      /// Does a move, but doesn't fully reset source (used for optimization) 
      template<class...T>
      concept HasAbandonConstructor = Validate<T...>
          and (HasIntentConstructor<::Langulus::Abandon, T> and ...);

      /// Check if all T have a dedicated refer-constructor                   
      /// Refering does a shallow copy while referencing contents             
      template<class...T>
      concept HasReferConstructor = Validate<T...>
          and (HasIntentConstructor<::Langulus::Refer, T> and ...);
      
      /// Check if all T have a dedicated copy-constructor                    
      /// Does a shallow copy _of the contents_ (it is like shallow cloning)  
      ///   @attention don't mistake it for the built-in copy-semantic        
      template<class...T>
      concept HasCopyConstructor = Validate<T...>
          and (HasIntentConstructor<::Langulus::Copy, T> and ...);

      /// Check if all T have a dedicated move-constructor                    
      /// Does a move, fully resetting source                                 
      template<class...T>
      concept HasMoveConstructor = Validate<T...>
          and (HasIntentConstructor<::Langulus::Move, T> and ...);

      /// Check if all T have a dedicated intent-assigner for S               
      ///   @tparam S the intent                                              
      ///   @tparam T the types                                               
      template<template<class> class S, class...T>
      concept HasIntentAssign = Validate<T...> and ((Intent<S<T>>
          and requires (T& lhs, S<T>&& rhs) { lhs = LglsFwd(rhs); }
         ) and ...);

      /// Check if all TypeOf<S> habe a dedicated intent-assigner for S       
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept HasIntentAssignAlt = Validate<S...> and ((Intent<S>
          and requires (Decvq<Deref<TypeOf<S>>>& lhs, S&& rhs) { lhs = LglsFwd(rhs); }
         ) and ...);

      /// Check if all T have a dedicated disown-assigner                     
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      template<class...T>
      concept HasDisownAssign = Validate<T...>
          and (HasIntentAssign<::Langulus::Disown, T> and ...);

      /// Check if all Decay<T> have a dedicated clone-assigner               
      /// Does a deep copy                                                    
      template<class...T>
      concept HasCloneAssign = Validate<T...>
          and (HasIntentAssign<::Langulus::Clone, T> and ...);

      /// Check if all T have a dedicated abandon-assigner                    
      /// Does a move, but doesn't fully reset source (optimization)          
      template<class...T>
      concept HasAbandonAssign = Validate<T...>
          and (HasIntentAssign<::Langulus::Abandon, T> and ...);

      /// Check if all T have a dedicated refer-assigner                      
      /// Refering does a shallow copy while referencing contents             
      template<class...T>
      concept HasReferAssign = Validate<T...>
          and (HasIntentAssign<::Langulus::Refer, T> and ...);
      
      /// Check if all T have a dedicated copy-assigner                       
      /// Does a shallow copy _of the contents_ (it is like shallow cloning)  
      ///   @attention don't mistake it for the built-in copy-semantic        
      template<class...T>
      concept HasCopyAssign = Validate<T...>
          and (HasIntentAssign<::Langulus::Copy, T> and ...);

      /// Check if all T have a dedicated move-assigner                       
      /// Does a move, fully resetting source                                 
      template<class...T>
      concept HasMoveAssign = Validate<T...>
          and (HasIntentAssign<::Langulus::Move, T> and ...);
   }

   
   /// Deduce the proper intent, based on whether T already has a             
   /// specified intent (like when it is an rvalue (&&))                      
   ///   - if it has one of those, then we get move intent                    
   ///   - if it isn't - we get refer intent                                  
   template<class T>
   using IntentOfT = Tif<CT::Intent<Decvq<Deref<T>>>,
         Decq<Deref<T>>,
         Tif<::std::is_rvalue_reference_v<T> and CT::Mutable<Deref<T>>,
            Move<Deref<T>>,
            Refer<Decq<Deref<T>>>
         >
      >;

   template<class T, class V> LANGULUS(ALWAYS_INLINED)
   constexpr auto DeduceIntent(V&& arg) noexcept {
      if constexpr (CT::Intent<Deref<T>>)
         return LglsFwd(arg);
      else if constexpr (::std::is_rvalue_reference_v<T> and not ::std::is_const_v<Deref<T>>)
         return Move {LglsFwd(arg)};
      else
         return Refer {arg};
   }
}

//#define IntentOf(a) ::Langulus::IntentOfT<decltype(a)>
//#define FWDIntent(a) IntentOf(a) {LglsFwd(a)} // for some reason this doesn't work on clang 22
#define FWDIntent(a) ::Langulus::DeduceIntent<decltype(a)>(LglsFwd(a))
#define IntentOf(a) decltype(FWDIntent(a)) /*::Langulus::Deref<decltype(FWDIntent(a))>*/
#define NestIntentOf(a, ...) IntentOf(a)::Nest(__VA_ARGS__) //::Langulus::Deref<decltype(FWDIntent(a))>::Nest(__VA_ARGS__)

/// A handy constructor & assignment pattern that adds all possible intents   
/// and collapses them for a given type. Useful when you don't want intents   
/// to get in the way of simple types that need those reflected, but not      
/// implemented in some particular way. Basically acts as "= default".        
/*#define ignore_all_intents(FOR_TYPE) \
   template<template<class> class I> requires ::Langulus::CT::Intent<I<FOR_TYPE>> \
   explicit constexpr FOR_TYPE(I<FOR_TYPE>&& meta) noexcept \
      : FOR_TYPE {*meta} {} \
   template<template<class> class I> requires ::Langulus::CT::Intent<I<FOR_TYPE>> \
   constexpr FOR_TYPE& operator = (I<FOR_TYPE>&& rhs) noexcept { \
      new (this) FOR_TYPE {*rhs}; \
      return *this; \
   }*/


namespace Langulus
{   
   /// MARK: IntentNew                                                        
   /// Create an instance of T at the provided memory using placement new     
   /// which considers the intent and checks if T's constructors support it.  
   /// All intent-related construction concepts are defined in terms of this  
   /// function. Beware, this is very unsafe - make sure all assumptions are  
   /// correct!                                                               
   ///   @attention assumes placement pointer is valid and is of type T       
   ///   @attention when S is a deep intent (like Clone) this function        
   ///      assumes that the 'placement' pointer always points to an          
   ///      instance of type Decay<T>                                         
   ///   @param placement where to place the new instance                     
   ///   @param intent the constructor argument and intent                    
   ///   @return the instance on the heap                                     
   template<bool FAKE = false, template<class> class S, CT::NoIntent T>
   requires CT::Intent<S<T>> LANGULUS(INLINED)
   constexpr auto IntentNew(void* placement, S<T>&& intent) {
      static_assert(    CT::Complete<T>,  "T has to be complete");
      static_assert(not CT::Reference<T>, "T can't be a reference");
      LglsAssumeDev(placement, "Invalid placement pointer");

      if constexpr (CT::Abstract<T>) {
         static_assert(FAKE, "Can't construct abstract type");
         return Unsupported {};
      }
      else if constexpr (CT::Referred<S<T>>) {
         // Refer                                                       
         if constexpr (CT::HasReferConstructor<T>)
            return new (placement) T {LglsFwd(intent)};
         else if constexpr (::std::copy_constructible<T>)
            return new (placement) T {intent.what};
         else {
            static_assert(FAKE, "Can't refer-construct type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Moved<S<T>>) {
         // Move                                                        
         if constexpr (CT::HasMoveConstructor<T>)
            return new (placement) T {LglsFwd(intent)};
         else if constexpr (::std::move_constructible<T>)
            return new (placement) T {LglsFwd(intent.what)};
         else {
            static_assert(FAKE, "Can't move-construct type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Abandoned<S<T>>) {
         // Abandon                                                     
         if constexpr (CT::HasAbandonConstructor<T>)
            return new (placement) T {LglsFwd(intent)};
         else if constexpr (CT::HasMoveConstructor<T>)
            return new (placement) T {Move(intent.what)};
         else if constexpr (::std::move_constructible<T>)
            return new (placement) T {LglsFwd(intent.what)};
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
         // @attention - cloning operates only on decayed types, and    
         //    if they're incomplete, then cloning is impossible        
         using DT = Decay<T>;
         if constexpr (not CT::Complete<DT>) {
            static_assert(FAKE, "Can't clone-construct an incomplete type");
            return Unsupported {};            
         }
         else if constexpr (CT::NotVoid<DT>) {
            if constexpr (CT::HasCloneConstructor<DT>)
               return new (placement) DT {Clone(DenseCast(intent.what))};
            else if constexpr (CT::POD<DT> and CT::HasReferConstructor<DT>)
               return new (placement) DT {Refer(DenseCast(intent.what))};
            else if constexpr (CT::POD<DT> and ::std::copy_constructible<DT>)
               return new (placement) DT {DenseCast(intent.what)};
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
            return new (placement) T {LglsFwd(intent)};
         else if constexpr (CT::POD<T> and CT::HasReferConstructor<T>)
            return new (placement) T {Refer(intent.what)};
         else if constexpr (CT::POD<T> and ::std::copy_constructible<T>)
            return new (placement) T {intent.what};
         else {
            static_assert(FAKE, "Can't copy-construct type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Disowned<S<T>>) {
         // Disown                                                      
         if constexpr (CT::HasDisownConstructor<T>)
            return new (placement) T {LglsFwd(intent)};
         else if constexpr (CT::POD<T> and CT::HasReferConstructor<T>)
            return new (placement) T {Refer(intent.what)};
         else if constexpr (CT::POD<T> and ::std::copy_constructible<T>)
            return new (placement) T {intent.what};
         else {
            static_assert(FAKE, "Can't disown-construct type");
            return Unsupported {};
         }
      }
      else static_assert(false, "Intent wasn't recognized");
   }

   /// MARK: IntentAssign                                                     
   /// Assign new value to an instance of T, using the provided intent        
   ///   @attention when S is a deep intent (like Clone) this function        
   ///      will DenseCast 'lhs' and 'rhs', and copy only dense data          
   ///   @param lhs left hand side (what are we assigning to)                 
   ///   @param rhs right hand side (what are we assigning)                   
   ///   @return whatever the assignment operator returns                     
   template<bool FAKE = false, CT::NoIntent T> LANGULUS(INLINED)
   constexpr decltype(auto) IntentAssign(T& lhs, CT::Intent auto&& rhs) {
      using S = IntentOf(rhs);
      static_assert(Same<T, Deint<S>>,
         "Argument doesn't match T");
      static_assert(CT::Mutable<T>,
         "T has to be mutable in order to be assigned");
      static_assert(CT::Complete<T>,
         "T has to be complete in order to be assigned");
      static_assert(not CT::Reference<T>,
         "T can't be a reference in order to be assigned");

      if constexpr (CT::Referred<S>) {
         // Refer                                                       
         if constexpr (CT::HasReferAssign<T>)
            return (lhs = LglsFwd(rhs));
         else if constexpr (::std::is_copy_assignable_v<T>)
            return (lhs = rhs.what);
         else {
            static_assert(FAKE, "Can't refer-assign type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Moved<S>) {
         // Move                                                        
         if constexpr (CT::HasMoveAssign<T>)
            return (lhs = LglsFwd(rhs));
         else if constexpr (::std::is_move_assignable_v<T>)
            return (lhs = LglsFwd(rhs.what));
         else {
            static_assert(FAKE, "Can't move-assign type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Abandoned<S>) {
         // Abandon                                                     
         if constexpr (CT::HasAbandonAssign<T>)
            return (lhs = LglsFwd(rhs));
         else if constexpr (CT::HasMoveAssign<T>)
            return (lhs = Move(rhs.what));
         else if constexpr (::std::is_move_assignable_v<T>)
            return (lhs = LglsFwd(rhs.what));
         else {
            static_assert(FAKE,
               "Can't abandon-assign destructible type"
               " - explicit abandon-assigner is required");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Cloned<S>) {
         // Clone                                                       
         // @attention - assumes that all levels of indirection have    
         //    been allocated and pointers point to valid memory        
         // @attention - cloning operates only on decayed types, and    
         //    if they're incomplete, then cloning is impossible        
         using DT = Decay<T>;

         if constexpr (not CT::Complete<DT>) {
            static_assert(FAKE, "Can't clone-assign incomplete type");
            return Unsupported {};            
         }   
         else if constexpr (CT::NotVoid<DT>) {
            if constexpr (CT::Mutable<Deptr<T>>) {
               if constexpr (CT::HasCloneAssign<DT>)
                  return (DenseCast(lhs) = Clone(DenseCast(rhs.what)));
               else if constexpr (CT::POD<DT> and CT::HasReferAssign<DT>)
                  return (DenseCast(lhs) = Refer(DenseCast(rhs.what)));
               else if constexpr (CT::POD<DT> and ::std::is_copy_assignable_v<DT>)
                  return (DenseCast(lhs) = DenseCast(rhs.what));
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
      else if constexpr (CT::Copied<S>) {
         // Copy                                                        
         if constexpr (CT::HasCopyAssign<T>)
            return (lhs = LglsFwd(rhs));
         else if constexpr (CT::POD<T> and CT::HasReferAssign<T>)
            return (lhs = Refer(rhs.what));
         else if constexpr (CT::POD<T> and ::std::is_copy_assignable_v<T>)
            return (lhs = rhs.what);
         else {
            static_assert(FAKE, "Can't copy-assign type");
            return Unsupported {};
         }
      }
      else if constexpr (CT::Disowned<S>) {
         // Disown                                                      
         if constexpr (CT::HasDisownAssign<T>)
            return (lhs = LglsFwd(rhs));
         else if constexpr (CT::POD<T> and CT::HasReferAssign<T>)
            return (lhs = Refer(rhs.what));
         else if constexpr (CT::POD<T> and ::std::is_copy_assignable_v<T>)
            return (lhs = rhs.what);
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
      concept AssignableFrom = requires (T t, A&&...a) { ((t = LglsFwd(a)), ...); };


      /// MARK: CT::Constructible                                             
      ///                                                                     
      ///   Constructibles                                                    
      ///                                                                     
      ///   These concepts are bit looser on requirements, compared to their  
      /// Has*Constructor counterparts to allow for fallbacks in places where 
      /// they are required. A type may not explicitly HasAbandonConstructor, 
      /// and yet be AbandonConstructible, because it is movable by the usual 
      /// C++20 semantics. Constructors are remarkably consistent across      
      /// compilers, unlike assignments (see below)...                        
      ///                                                                     

      /// Check if all T are intent-constructible by intent S.                
      /// T can be intent-constructible even if not having the specific       
      /// constructor, as long as T and S are compatible with standard C++20  
      /// semantics.                                                          
      ///   @tparam S the intent                                              
      ///   @tparam T the types                                               
      template<template<class> class S, class...T>
      concept IntentConstructible = NotVoid<T...> and Intent<S<T>...>
          and requires (S<T>&&...a) {
             {(IntentNew<true>(nullptr, LglsFwd(a)), ...)} -> Supported;
          };

      /// Check if all TypeOf<S> are intent-constructible by intent S.        
      /// T can be intent-constructible even if not having the specific       
      /// constructor, as long as T and S are compatible with standard C++20  
      /// semantics.                                                          
      ///   @tparam S the intents and types                                   
      /*template<class...S>
      concept IntentConstructibleAlt = Intent<S...>
          and requires (S&&...a) {
             {(IntentNew<true>(nullptr, LglsFwd(a)), ...)} -> Supported;
          };*/

      /// Check if all T are disown-constructible.                            
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      /// If POD, T can be disown-constructible even if not having the        
      /// specific disown constructor, as long as T is std::copy_constuctible.
      template<class...T>
      concept DisownConstructible = Validate<T...>
          and (IntentConstructible<Langulus::Disown, T> and ...);

      /// Check if all Decay<T> are clone-constructible.                      
      /// Does a deep copy. If POD, Decay<T> can be clone-constructible even  
      /// if not having the specific clone constructor, as long as T is       
      /// std::copy_constuctible.                                             
      template<class...T>
      concept CloneConstructible = Validate<T...>
          and (IntentConstructible<Langulus::Clone, T> and ...);

      /// Check if all T are abandon-constructible.                           
      /// Does a move but doesn't fully reset source as an optimization -     
      /// assuming that the abandoned instance is never going to be used in   
      /// other ways than just calling the destructor. T can be               
      /// abandon-constructible even if not having the specific abandon       
      /// constructor, as long as it is std::move_constuctible.               
      template<class...T>
      concept AbandonConstructible = Validate<T...>
          and (IntentConstructible<Langulus::Abandon, T> and ...);

      /// Check if all T are refer-constructible.                             
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership. T can be refer-constructible as long as T is             
      /// std::copy_constuctible.                                             
      template<class...T>
      concept ReferConstructible = Validate<T...>
          and (IntentConstructible<Langulus::Refer, T> and ...);
      
      /// Check if all T are copy-constructible.                              
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      /// If POD, T can be copy-constructible even if not having the specific 
      /// shallow-copy constructor, as long as T is std::copy_constuctible.   
      template<class...T>
      concept CopyConstructible = Validate<T...>
          and (IntentConstructible<Langulus::Copy, T> and ...);

      /// Check if all T are move-constructible.                              
      /// Does a move, fully resetting source into a reusable state.          
      /// T is move-constructible as long as it is std::move_constuctible.    
      template<class...T>
      concept MoveConstructible = Validate<T...>
          and (IntentConstructible<Langulus::Move, T> and ...);


      /// MARK: CT::Assignable                                                
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
      ///   that.                                                             
      /// - it causes ambiguity on GCC 14.2 for move/abandon intents, because 
      ///   the compiler can't decide how to implicit-cast to && or           
      ///   const&. I've added explicit intent assigners to compensate for    
      ///   that.                                                             
      /// - there is also this nasty compiler bug on MSVC v143 that affects   
      ///   types with deleted destructors, and implicit copy/move semantics  
      ///   https://stackoverflow.com/questions/79665049                      
      ///                                                                     
      /// Keep in mind, that none of these concepts here guarantee, that an   
      /// adequate intent-assignment exists for a type, unless you use        
      /// IntentAssign itself, instead of operator=. Implicit mapping onto    
      /// built-in copy/move semantics has been disabled to avoid all the     
      /// aforementioned inconsistencies across compilers.                    
      ///                                                                     

      /// Check if all T are intent-assignable by intent S.                   
      /// T can be intent-assignable even if not having an explicit assigner, 
      /// as long as T and S are compatible with the usual C++20 semantics.   
      ///   @tparam S the intent                                              
      ///   @tparam T the types                                               
      template<template<class> class S, class...T>
      concept IntentAssignable = NotVoid<T...> and Mutable<T...>
          and Intent<S<Decvq<T>>...> and requires (S<Decvq<T>>&&...a) {
            {(IntentAssign<true>(LglsFake(Decvq<T>&), LglsFwd(a)), ...)} -> Supported;
          };

      /// Check if all TypeOf<S> are intent-assignable by S.                  
      /// T can be intent-assignable even if not having an explicit assigner  
      /// as long as T and S are compatible with standard C++20 semantics.    
      ///   @tparam S - the intent and type                                   
      /*template<class...S>
      concept IntentAssignableAlt = Intent<S...> and requires (S&&...a) {
            {(IntentAssign<true>(LglsFake(Decq<Deref<TypeOf<S>>>&), LglsFwd(a)), ...)} -> Supported;
          };*/

      /// Check if all T are disown-assignable.                               
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      /// If POD, T can be disown-assignable even if not having an explicit   
      /// disown-assignment, as long as std::copy_assignable<T> holds.        
      template<class...T>
      concept DisownAssignable = Validate<T...>
          and (IntentAssignable<Langulus::Disown, T> and ...);

      /// Check if all Decay<T> are clone-assignable.                         
      /// Does a deep copy.                                                   
      /// If POD, Decay<T> can be clone-assignable even if not having an      
      /// explicit clone-assignment, as long as std::copy_assignable<T> holds.
      template<class...T>
      concept CloneAssignable = Validate<T...>
          and (IntentAssignable<Langulus::Clone, T> and ...);

      /// Check if all T are abandon-assignable.                              
      /// Does a move, but doesn't fully reset source (optimization).         
      /// T can be abandon-assignable even if not having an explicit          
      /// abandon-assignment, as long as std::move_assignable<T> holds.       
      template<class...T>
      concept AbandonAssignable = Validate<T...>
          and (IntentAssignable<Langulus::Abandon, T> and ...);

      /// Check if all T are refer-assignable.                                
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership.                                                          
      /// T can be refer-assignable as long as std::copy_assignable<T> holds. 
      template<class...T>
      concept ReferAssignable = Validate<T...>
          and (IntentAssignable<Langulus::Refer, T> and ...);
      
      /// Check if all T are copy-assignable.                                 
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      /// If POD, T can be copy-assignable even if not having an explicit     
      /// copy-assigner, as long as std::copy_assignable<T> holds.            
      template<class...T>
      concept CopyAssignable = Validate<T...>
          and (IntentAssignable<Langulus::Copy, T> and ...);

      /// Check if all T are move-assignable.                                 
      /// Does a move, fully resetting source.                                
      /// T is move-assignable as long as std::move_assignable<T> holds.      
      template<class...T>
      concept MoveAssignable = Validate<T...>
          and (IntentAssignable<Langulus::Move, T> and ...);
   }
}
