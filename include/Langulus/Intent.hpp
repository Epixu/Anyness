///                                                                           
/// Langulus::Core                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "CTTI.hpp"
#include "CT/Derived.hpp"
#include "CT/POD.hpp"


namespace Langulus::CTTI
{

   /// Can be used in two ways to satisfy CT::Intent<T>:                      
   /// 1. Specialize for T/concept                                            
   /// 2. Add a public `using CTTI_Intent = Yes/No;` in T                     
   template<class T>
   struct Intent {
      static constexpr bool Enabled = false;
   };
   
} // namespace Langulus::CTTI

namespace Langulus::CT
{
   template<class...T>
   concept Intent = ((CTTI::Intent<T>::Enabled or T::CTTI_Intent::Enabled) and ...);

   template<class...T>
   concept NoIntent = ((not Intent<T>) and ...);

} // namespace Langulus::CT

namespace Langulus
{
   namespace Anyness
   {
      struct Many;
   }


   ///                                                                        
   /// Referred value intermediate type, use in constructors and assignments  
   /// to refer to data explicitly                                            
   ///   @tparam T - the type to refer                                        
   template<class T>
   struct Referred {
   private:
      const T& mValue;

   public:
      using CTTI_Typed = T;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract = Yes;
      using CTTI_Unallocatable = Yes;
      using CTTI_Intent = Yes;
      using CTTI_Sheddable = Yes;

      static constexpr bool Shallow = true;
      static constexpr bool Keep = true;
      static constexpr bool Move = false;
      static constexpr bool ResetsOnMove = Keep and Move;

      Referred() = delete;
      explicit constexpr Referred(const Referred&) noexcept = default;
      explicit constexpr Referred(Referred&&) noexcept = default;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Referred(const T& value) noexcept
         : mValue {value} {
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
            return Referred<ALT_T> {mValue};
      }

      /// Refer something else                                                
      ///   @param value - the value to refer (can be an intent)              
      ///   @return the referred value, disregarding previous intent          
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(auto&& value) noexcept {
         using ALT = Decq<Deref<decltype(value)>>;

         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Intent<ALT>) {
            using ALT_T = TypeOf<ALT>;

            if constexpr (CT::Aggregate<ALT_T>)
               return *value;
            else
               return Referred<TypeOf<ALT>> {*value};
         }
         else {
            if constexpr (CT::Aggregate<ALT>)
               return value;
            else
               return Referred<ALT> {value};
         }
      }

      template<class ALT>
      using As = ::std::conditional_t<CT::Intent<ALT>, Referred<TypeOf<ALT>>, Referred<ALT>>;

      LANGULUS(ALWAYS_INLINED)
      const T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      auto operator -> () const noexcept {
         if constexpr (CT::Sparse<T>)
            return mValue;
         else
            return &mValue;
      }

      /// Implicitly collapse the intent                                      
      /// This way this wrapper is seamlessly integrated with the standard    
      /// C++20 copy semantics                                                
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () const& noexcept {
         return mValue;
      }
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () & noexcept {
         return mValue;
      }
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () && noexcept {
         return mValue;
      }
   };
   
   /// Refer a value                                                          
   LANGULUS(ALWAYS_INLINED)
   constexpr auto Refer(auto&& value) noexcept {
      using ALT = Decq<Deref<decltype(value)>>;
      if constexpr (CT::Intent<ALT>)
         return Referred<TypeOf<ALT>> {*value};
      else
         return Referred<ALT> {value};
   }
   
   
   ///                                                                        
   /// Copied value intermediate type, use in constructors and assignments    
   /// to shallow-copy container explicitly                                   
   ///   @tparam T - the type to copy                                         
   template<class T>
   struct Copied {
   private:
      const T& mValue;

   public:
      using CTTI_Typed = T;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract = Yes;
      using CTTI_Unallocatable = Yes;
      using CTTI_Intent = Yes;
      using CTTI_Sheddable = Yes;

      static constexpr bool Shallow = true;
      static constexpr bool Keep = true;
      static constexpr bool Move = false;
      static constexpr bool ResetsOnMove = Keep and Move;

      Copied() = delete;
      constexpr Copied(const Copied&) noexcept = default;
      explicit constexpr Copied(Copied&&) noexcept = default;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Copied(const T& value) noexcept
         : mValue {value} {
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
            return Copied<ALT_T> {mValue};
      }

      /// Copy something else                                                 
      ///   @param value - the value to copy (can be an intent)               
      ///   @return the copied value, disregarding previous intent            
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(auto&& value) noexcept {
         using ALT = Decq<Deref<decltype(value)>>;

         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Intent<ALT>) {
            using ALT_T = TypeOf<ALT>;

            if constexpr (CT::Aggregate<ALT_T>)
               return *value;
            else
               return Copied<TypeOf<ALT>> {*value};
         }
         else {
            if constexpr (CT::Aggregate<ALT>)
               return value;
            else
               return Copied<ALT> {value};
         }
      }

      template<class ALT>
      using As = ::std::conditional_t<CT::Intent<ALT>, Copied<TypeOf<ALT>>, Copied<ALT>>;

      LANGULUS(ALWAYS_INLINED)
      const T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      auto operator -> () const noexcept {
         if constexpr (CT::Sparse<T>)
            return mValue;
         else
            return &mValue;
      }

      /// Implicitly collapse the intent, when applying it to POD/Sparse,     
      /// since Refer is isomorphic to Copy in those cases                    
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () const noexcept
      requires (CT::POD<T> or CT::Sparse<T>) {
         return mValue;
      }

      /// Used by DecayCast                                                   
      LANGULUS(ALWAYS_INLINED)
      constexpr const T& TypedCast() const noexcept {
         return mValue;
      }
   };
   
   /// Copy a value                                                           
   LANGULUS(ALWAYS_INLINED)
   constexpr auto Copy(auto&& value) noexcept {
      using ALT = Decq<Deref<decltype(value)>>;
      if constexpr (CT::Intent<ALT>)
         return Copied<TypeOf<ALT>> {*value};
      else
         return Copied<ALT> {value};
   }
   

   ///                                                                        
   /// Moved value intermediate type, use in constructors and assignments     
   /// to move data explicitly                                                
   ///   @tparam T - the type to move                                         
   template<class T>
   struct Moved {
   protected:
      T&& mValue;

   public:
      using CTTI_Typed = T;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract = Yes;
      using CTTI_Unallocatable = Yes;
      using CTTI_Intent = Yes;
      using CTTI_Sheddable = Yes;

      static constexpr bool Shallow = true;
      static constexpr bool Keep = true;
      static constexpr bool Move = true;
      static constexpr bool ResetsOnMove = Keep and Move;

      Moved() = delete;
      constexpr Moved(const Moved& r) noexcept
         : mValue {::std::forward<T>(r.mValue)} {}
      explicit constexpr Moved(Moved&&) noexcept = default;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Moved(T& value) noexcept
         : mValue {::std::move(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Moved(T&& value) noexcept
         : mValue {::std::forward<T>(value)} {
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
            return ::std::forward<ALT_T>(mValue);
         else
            return Moved<ALT_T> {::std::forward<ALT_T>(mValue)};
      }

      /// Move something else                                                 
      ///   @param value - the value to move (can be an intent)               
      ///   @return the moved value, disregarding previous intent             
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(auto&& value) noexcept {
         using ALT = Decq<Deref<decltype(value)>>;

         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard move semantics                  
         if constexpr (CT::Intent<ALT>) {
            using ALT_T = TypeOf<ALT>;

            if constexpr (CT::Aggregate<ALT_T>)
               return ::std::forward<ALT_T>(*value);
            else
               return Moved<ALT_T> {::std::forward<ALT_T>(*value)};
         }
         else {
            if constexpr (CT::Aggregate<ALT>)
               return ::std::forward<ALT>(value);
            else
               return Moved<ALT> {::std::forward<ALT>(value)};
         }
      }

      template<class ALT>
      using As = ::std::conditional_t<CT::Intent<ALT>, Moved<TypeOf<ALT>>, Moved<ALT>>;

      LANGULUS(ALWAYS_INLINED)
      T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      auto operator -> () const noexcept {
         if constexpr (CT::Sparse<T>)
            return mValue;
         else
            return &mValue;
      }

      /// Implicitly collapse the intent                                      
      /// This way this wrapper is seamlessly integrated with the standard    
      /// C++20 move-semantics                                                
      LANGULUS(ALWAYS_INLINED)
      constexpr operator T&& () const noexcept {
         return ::std::forward<T>(mValue);
      }

      LANGULUS(ALWAYS_INLINED)
      constexpr operator T&& () noexcept {
         return ::std::forward<T>(mValue);
      }

      /// Used by DecayCast                                                   
      LANGULUS(ALWAYS_INLINED)
      constexpr T& TypedCast() const noexcept {
         return mValue;
      }
   };
   
   /// Move a value                                                           
   LANGULUS(ALWAYS_INLINED)
   constexpr auto Move(auto&& value) noexcept {
      using ALT = Decq<Deref<decltype(value)>>;
      if constexpr (CT::Intent<ALT>)
         return Moved<TypeOf<ALT>> {::std::forward<TypeOf<ALT>>(*value)};
      else
         return Moved<ALT> {::std::forward<ALT>(value)};
   }


   ///                                                                        
   /// Abandoned value intermediate type, can be used in constructors and     
   /// assignments to provide a guarantee, that the value shall not be used   
   /// after that function, so we can save up on resetting it fully           
   /// For example, you can construct a Many with an abandoned Many, which is 
   /// same as move-construction, but the abandoned Many shall have only its  
   /// mEntry reset, instead of the entire container.                         
   ///   @tparam T - the type to abandon                                      
   template<class T>
   struct Abandoned {
   protected:
      T&& mValue;

   public:
      using CTTI_Typed = T;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract = Yes;
      using CTTI_Unallocatable = Yes;
      using CTTI_Intent = Yes;
      using CTTI_Sheddable = Yes;

      static constexpr bool Shallow = true;
      static constexpr bool Keep = false;
      static constexpr bool Move = true;
      static constexpr bool ResetsOnMove = Keep and Move;

      Abandoned() = delete;
      constexpr Abandoned(const Abandoned& r) noexcept
         : mValue {::std::forward<T>(r.mValue)} {}
      explicit constexpr Abandoned(Abandoned&&) noexcept = default;
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Abandoned(T& value) noexcept
         : mValue {::std::move(value)} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }
      
      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Abandoned(T&& value) noexcept
         : mValue {::std::forward<T>(value)} {
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
            return ::std::forward<ALT_T>(mValue);
         else
            return Abandoned<ALT_T> {::std::forward<ALT_T>(mValue)};
      }

      /// Abandon something else                                              
      ///   @param value - the value to abandon (can be an intent)            
      ///   @return the abandoned value, disregarding previous intent         
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(auto&& value) noexcept {
         using ALT = Decq<Deref<decltype(value)>>;

         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard move semantics                  
         if constexpr (CT::Intent<ALT>) {
            using ALT_T = TypeOf<ALT>;

            if constexpr (CT::Aggregate<ALT_T>)
               return ::std::forward<ALT_T>(*value);
            else
               return Abandoned<ALT_T> {::std::forward<ALT_T>(*value)};
         }
         else {
            if constexpr (CT::Aggregate<ALT>)
               return ::std::forward<ALT>(value);
            else
               return Abandoned<ALT> {::std::forward<ALT>(value)};
         }
      }

      template<class ALT>
      using As = ::std::conditional_t<CT::Intent<ALT>, Abandoned<TypeOf<ALT>>, Abandoned<ALT>>;

      LANGULUS(ALWAYS_INLINED)
      T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      auto operator -> () const noexcept {
         if constexpr (CT::Sparse<T>)
            return mValue;
         else
            return &mValue;
      }

      /// Implicitly collapse the intent                                      
      /// This way this wrapper is seamlessly integrated with the standard    
      /// C++20 move-semantics                                                
      LANGULUS(ALWAYS_INLINED)
      constexpr operator T&& () const noexcept {
         return ::std::forward<T>(mValue);
      }

      /// Used by DecayCast                                                   
      LANGULUS(ALWAYS_INLINED)
      constexpr T& TypedCast() const noexcept {
         return mValue;
      }
   };
   
   /// Abandon a value                                                        
   /// Same as Move, but resets only mandatory data inside source after move  
   /// essentially saving up on a couple of instructions                      
   LANGULUS(ALWAYS_INLINED)
   constexpr auto Abandon(auto&& value) noexcept {
      using ALT = Decq<Deref<decltype(value)>>;
      if constexpr (CT::Intent<ALT>)
         return Abandoned<TypeOf<ALT>> {::std::forward<TypeOf<ALT>>(*value)};
      else
         return Abandoned<ALT>         {::std::forward<ALT>(value)};
   }


   ///                                                                        
   /// Disowned value intermediate type, use in constructors and assignments  
   /// to copy container without gaining ownership                            
   ///   @tparam T - the type to disown                                       
   template<class T>
   struct Disowned {
   protected:
      const T& mValue;

   public:
      using CTTI_Typed = T;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract = Yes;
      using CTTI_Unallocatable = Yes;
      using CTTI_Intent = Yes;
      using CTTI_Sheddable = Yes;

      static constexpr bool Shallow = true;
      static constexpr bool Keep = false;
      static constexpr bool Move = false;
      static constexpr bool ResetsOnMove = Keep and Move;

      Disowned() = delete;
      constexpr Disowned(const Disowned&) noexcept = default;
      explicit constexpr Disowned(Disowned&&) noexcept = default;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Disowned(const T& value) noexcept
         : mValue {value} {
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
            return Disowned<ALT_T> {mValue};
      }

      /// Disown something else                                               
      ///   @param value - the value to disown (can be an intent)             
      ///   @return the disowned value, disregarding previous intent          
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(auto&& value) noexcept {
         using ALT = Decq<Deref<decltype(value)>>;

         // Aggregates don't play well with intents, so if type is an   
         // aggregate, use the standard copy semantics                  
         if constexpr (CT::Intent<ALT>) {
            using ALT_T = TypeOf<ALT>;

            if constexpr (CT::Aggregate<ALT_T>)
               return *value;
            else
               return Disowned<TypeOf<ALT>> {*value};
         }
         else {
            if constexpr (CT::Aggregate<ALT>)
               return value;
            else
               return Disowned<ALT> {value};
         }
      }

      template<class ALT>
      using As = ::std::conditional_t<CT::Intent<ALT>, Disowned<TypeOf<ALT>>, Disowned<ALT>>;
      
      LANGULUS(ALWAYS_INLINED)
      const T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      auto operator -> () const noexcept {
         if constexpr (CT::Sparse<T>)
            return mValue;
         else
            return &mValue;
      }

      /// Implicitly collapse the intent, when applying it to PODs,           
      /// since they are never allowed to have ownership either way           
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () const noexcept requires CT::POD<T> {
         return mValue;
      }

      /// Used by DecayCast                                                   
      LANGULUS(ALWAYS_INLINED)
      constexpr const T& TypedCast() const noexcept {
         return mValue;
      }
   };
   
   /// Disown a value                                                         
   /// Same as a shallow-copy, but never references, saving some instructions 
   LANGULUS(ALWAYS_INLINED)
   constexpr auto Disown(auto&& value) noexcept {
      using ALT = Decq<Deref<decltype(value)>>;
      if constexpr (CT::Intent<ALT>)
         return Disowned<TypeOf<ALT>> {*value};
      else
         return Disowned<ALT> {value};
   }
   

   ///                                                                        
   /// Cloned value intermediate type, used in constructors and assignments   
   /// to clone container, doing a deep copy instead of default shallow one   
   ///   @tparam T - the type to clone                                        
   template<class T>
   struct Cloned {
   protected:
      const T& mValue;

   public:
      using CTTI_Typed = T;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract = Yes;
      using CTTI_Unallocatable = Yes;
      using CTTI_Intent = Yes;
      using CTTI_Sheddable = Yes;

      static constexpr bool Shallow = false;
      static constexpr bool Keep = true;
      static constexpr bool Move = false;
      static constexpr bool ResetsOnMove = Keep and Move;

      Cloned() = delete;
      constexpr Cloned(const Cloned&) noexcept = default;
      explicit constexpr Cloned(Cloned&&) noexcept = default;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Cloned(const T& value) noexcept
         : mValue {value} {
         static_assert(CT::NoIntent<T>, "Can't nest intents");
      }

      /// Forward as cloned, never collapse                                   
      template<class ALT_T = T> LANGULUS(ALWAYS_INLINED)
      constexpr decltype(auto) Forward() const noexcept {
         static_assert(CT::NoIntent<ALT_T>,
            "Can't nest intents");
         static_assert(CT::DerivedFrom<T, ALT_T>,
            "Can't forward as this type");
         return Cloned<ALT_T> {mValue};
      }

      /// Clone something else                                                
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(auto&& value) noexcept {
         using ALT = Decq<Deref<decltype(value)>>;
         if constexpr (CT::Intent<ALT>)
            return Cloned<TypeOf<ALT>> {*value};
         else
            return Cloned<ALT> {value};
      }

      template<class ALT>
      using As = ::std::conditional_t<CT::Intent<ALT>, Cloned<TypeOf<ALT>>, Cloned<ALT>>;
      
      LANGULUS(ALWAYS_INLINED)
      const T& operator * () const noexcept { return mValue; }

      LANGULUS(ALWAYS_INLINED)
      auto operator -> () const noexcept {
         if constexpr (CT::Sparse<T>)
            return mValue;
         else
            return &mValue;
      }

      /// Implicitly collapse the intent, when applying it to PODs,           
      /// since they are always cloned upon copy (ONLY IF `T` IS DENSE)       
      LANGULUS(ALWAYS_INLINED)
      constexpr operator const T& () const noexcept
      requires (CT::POD<T> and CT::Dense<T>) {
         return mValue;
      }

      /// Used by DecayCast                                                   
      LANGULUS(ALWAYS_INLINED)
      constexpr const T& TypedCast() const noexcept {
         return mValue;
      }
   };
   
   /// Clone a value                                                          
   /// Does a deep-copy                                                       
   LANGULUS(ALWAYS_INLINED)
   constexpr auto Clone(auto&& value) noexcept {
      using ALT = Decq<Deref<decltype(value)>>;
      if constexpr (CT::Intent<ALT>)
         return Cloned<TypeOf<ALT>> {*value};
      else
         return Cloned<ALT> {value};
   }


   ///                                                                        
   /// Descriptor intermediate type, used in constructors and assignment      
   /// operators to enable descriptor construction/assignment. The inner type 
   /// is always a reference to a type-erased container.                      
   struct Describe {
   protected:
      using Many = Anyness::Many;
      const Many& mValue;

   public:
      using CTTI_Typed = Many;
      using CTTI_ReflectAs = void;
      using CTTI_Abstract = Yes;
      using CTTI_Unallocatable = Yes;

      Describe() = delete;
      constexpr Describe(const Describe&) noexcept = default;
      explicit constexpr Describe(Describe&&) noexcept = default;

      LANGULUS(ALWAYS_INLINED)
      explicit constexpr Describe(const Many& value) noexcept
         : mValue {value} {}

      /// Forward as descibe                                                  
      LANGULUS(ALWAYS_INLINED)
      constexpr Describe&& Forward() noexcept {
         return static_cast<Describe&&>(*this);
      }

      /// The describe intent completely ignores nesting, only propagates     
      /// itself                                                              
      LANGULUS(ALWAYS_INLINED)
      static constexpr decltype(auto) Nest(auto&& value) noexcept {
         using ALT = Decq<Deref<decltype(value)>>;
         if constexpr (CT::Similar<ALT, Describe>)
            return ::std::forward<ALT>(value);
         else if constexpr (CT::Intent<ALT> and CT::Similar<TypeOf<ALT>, Many>)
            return Describe {*value};
         else if constexpr (CT::Similar<ALT, Many>)
            return Describe {value};
         else
            static_assert(false, "Can't nest provided type as a Describe semantic");
      }

      LANGULUS(ALWAYS_INLINED)
      const auto& operator *  () const noexcept { return  mValue; }

      LANGULUS(ALWAYS_INLINED)
      const auto* operator -> () const noexcept { return &mValue; }
   };

   
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
      concept HasIntentConstructor = Complete<T...> and Intent<S<T>...>
          and requires (S<T>&&...a) { (T (Forward<S<T>>(a)), ...); };

      /// Check if all TypeOf<S> have intent constructors for S               
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept HasIntentConstructorAlt = Complete<TypeOf<S>...> and Intent<S...>
          and requires (S&&...a) { (TypeOf<S> (Forward<S>(a)), ...); };

      /// Check if all T have a disown-constructor                            
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      template<class...T>
      concept HasDisownConstructor = (HasIntentConstructor<Disowned, T> and ...);

      /// Check if all Decay<T> have a clone-constructor                      
      /// Does a deep copy                                                    
      template<class...T>
      concept HasCloneConstructor = (HasIntentConstructor<Cloned, T> and ...);

      /// Check if all T have a abandon-constructor                           
      /// Does a move, but doesn't fully reset source (optimization)          
      template<class...T>
      concept HasAbandonConstructor = (HasIntentConstructor<Abandoned, T> and ...);

      /// Check if all T have a refer-constructor                             
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership.                                                          
      /// T has refer-constructor as long as it is std::copy_constuctible     
      template<class...T>
      concept HasReferConstructor = Complete<T...>
          and ((HasIntentConstructor<Referred, T>
           or ::std::copy_constructible<T>) and ...);
      
      /// Check if all T have a copy-constructor (don't mistake it for a      
      /// std::copy_constructible!)                                           
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      template<class...T>
      concept HasCopyConstructor = (HasIntentConstructor<Copied, T> and ...);

      /// Check if all T have a move-constructor                              
      /// Does a move, fully resetting source                                 
      /// T has move-constructor as long as it is std::move_constuctible      
      template<class...T>
      concept HasMoveConstructor = Complete<T...> and ((Sparse<T>
           or HasIntentConstructor<Moved, T>
           or ::std::move_constructible<T>) and ...);

      /// Check if all T have an intent-assigner for S                        
      ///   @tparam S - the intent                                            
      ///   @tparam T... - the types                                          
      template<template<class> class S, class...T>
      concept HasIntentAssign = Complete<T...> and ((Intent<S<T>>
          and ::std::assignable_from<T&, S<T>&&>) and ...);

      /// Check if all TypeOf<S> has intent-assigner for S                    
      ///   @tparam S - the intent and type                                   
      template<class...S>
      concept HasIntentAssignAlt = Complete<TypeOf<S>...> and ((Intent<S>
          and ::std::assignable_from<TypeOf<S>&, S&&>) and ...);

      /// Check if all T have a disown-assigner                               
      /// Disowning does a shallow copy without referencing contents,         
      /// generating a 'view' of the data that is without ownership.          
      template<class...T>
      concept HasDisownAssign = (HasIntentAssign<Disowned, T> and ...);

      /// Check if all Decay<T> have a clone-assigner                         
      /// Does a deep copy                                                    
      template<class...T>
      concept HasCloneAssign = (HasIntentAssign<Cloned, T> and ...);

      /// Check if all T have an abandon-assigner                             
      /// Does a move, but doesn't fully reset source (optimization)          
      template<class...T>
      concept HasAbandonAssign = (HasIntentAssign<Abandoned, T> and ...);

      /// Check if all T have refer-assigner                                  
      /// Refering does a shallow copy while referencing contents, providing  
      /// ownership.                                                          
      /// T has a refer-assigner as long as std::copy_assignable<T> holds     
      template<class...T>
      concept HasReferAssign = Complete<T...>
          and ((HasIntentAssign<Referred, T>
           or ::std::assignable_from<T&, const T&>) and ...);
      
      /// Check if all T have a copy-assigner (don't mistake it for a         
      /// std::copy_assignable!)                                              
      /// Does a shallow copy _of the contents_ (like shallow cloning).       
      template<class...T>
      concept HasCopyAssign = (HasIntentAssign<Copied, T> and ...);

      /// Check if all T have a move-assigner                                 
      /// Does a move, fully resetting source                                 
      /// T has a move-assigner as long as std::assignable_from<T&, T&&> holds
      /// This includes the cases when the type has a default copy-assign to  
      /// which the compiler falls back to. In that case move-assignment is   
      /// the same as refer-assignment.                                       
      template<class...T>
      concept HasMoveAssign = Complete<T...>
          and ((HasIntentAssign<Moved, T>
           or ::std::assignable_from<T&, T&&>) and ...);

   } // namespace Langulus::CT

   
   /// Deduce the proper intent, based on whether T already has a             
   /// specified intent, is an rvalue (&&), or none of those                  
   /// If it has one of those, then we get move intent; if it isn't - we      
   /// get refer intent (which can fallback to copy semantics)                
   template<class T>
   using IntentOf = ::std::conditional_t<
         CT::Intent<T>,
         Decay<T>,
         ::std::conditional_t<
            ::std::is_rvalue_reference_v<T> and CT::Mutable<Deref<T>>,
            Moved<Deref<T>>,
            Referred<Deref<T>>
         >
      >;

   /// Shed the intent from a type, if any                                    
   template<class T>
   using Deint = ::std::conditional_t<CT::Intent<T>, TypeOf<T>, T>;

   /// Decay an intent to the contained data                                  
   ///   @param what - the instance to decay                                  
   ///   @return a reference (preferably) or a copy of the inner data         
   LANGULUS(ALWAYS_INLINED)
   constexpr auto& DeintCast(auto&& what) noexcept {
      using T = decltype(what);
      if constexpr (CT::Intent<T>)
         return TypedCast(Forward<T>(what));
      else
         return what;
   }

} // namespace Langulus
