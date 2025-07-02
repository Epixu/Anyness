///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Meta.hpp"
#include "DefinitionVerb.hpp"


namespace Langulus::RTTI
{
   namespace Inner
   {
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      ///                                                                     
      /// Tehse rely on the definition limits to pack an ID into the smallest 
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      ///                                                                     
      
      /// Packing strategy that can't exceed 2^(8*ID_SIZE)-2 possible verbs   
      template<unsigned ID_SIZE>
      struct MetaVerbStructured_X8 : MetaPacked<ID_SIZE> {
      private:
         union {
            struct {
               // The set of the main properties                        
               bool reversible   : 1;
               bool constant     : 1;
               bool defaultable  : 1;
               bool stateless    : 1;
            };
            uint8_t all {};
         };

      public:
         using Base = MetaPacked<ID_SIZE>;

         constexpr MetaVerbStructured_X8() noexcept = default;
         constexpr MetaVerbStructured_X8(MetaVerbStructured_X8 const&) noexcept = default;
         constexpr MetaVerbStructured_X8(MetaVerbStructured_X8&&) noexcept = default;
         explicit constexpr MetaVerbStructured_X8(nullptr_t) noexcept;
         explicit constexpr MetaVerbStructured_X8(DefinitionVerb const*) noexcept;

         constexpr MetaVerbStructured_X8& operator = (MetaVerbStructured_X8 const&) noexcept = default;
         constexpr MetaVerbStructured_X8& operator = (MetaVerbStructured_X8&&) noexcept = default;
         constexpr MetaVerbStructured_X8& operator = (nullptr_t) noexcept;
         constexpr MetaVerbStructured_X8& operator = (DefinitionVerb const*) noexcept;

         auto GetPositiveName()     const noexcept -> Token;
         auto GetNegativeName()     const noexcept -> Token;
         auto GetPositiveOperator() const noexcept -> Token;
         auto GetNegativeOperator() const noexcept -> Token;
         auto GetBoundaries()       const noexcept -> Definition::BoundarySet const&;

         constexpr bool IsReversible()  const noexcept;
         constexpr bool IsConstant()    const noexcept;
         constexpr bool IsMutable()     const noexcept;
         constexpr bool IsDefaultable() const noexcept;
         constexpr bool IsStateless()   const noexcept;
      };
   #endif
      static_assert(sizeof(MetaVerbStructured_X8<1>) == 2);
      static_assert(sizeof(MetaVerbStructured_X8<2>) == 3);
      static_assert(sizeof(MetaVerbStructured_X8<3>) == 4);

      /// A naked pointer to a definition. Probably the fastest, but most     
      /// memory-inefficient on 64bit systems                                 
      struct MetaVerbNaked : MetaNaked<DefinitionVerb> {
         using Base = MetaNaked<DefinitionVerb>;

         using Base::Base;
         using Base::operator =;
         using Base::operator bool;

         auto GetPositiveName() const noexcept -> Token;
         auto GetNegativeName() const noexcept -> Token;
         auto GetPositiveOperator() const noexcept -> Token;
         auto GetNegativeOperator() const noexcept -> Token;

         #if LANGULUS_FEATURE(MANAGED_REFLECTION)
            auto GetBoundaries() const noexcept -> Definition::BoundarySet const&;
         #endif

         constexpr bool IsReversible() const noexcept;
         constexpr bool IsConstant() const noexcept;
         constexpr bool IsMutable() const noexcept;
         constexpr bool IsDefaultable() const noexcept;
         constexpr bool IsStateless() const noexcept;
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaVerbBase = MetaVerbStructured_X8<1>;
   #else
      using MetaVerbBase = MetaVerbNaked;
   #endif

   } // namespace Langulus::RTTI::Inner


   ///                                                                        
   ///   Verb type ID                                                         
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// either packed to a smaller size, or carry a lot of meta information    
   /// in the ID itself to avoid indirection - all this is configurable.      
   ///                                                                        
   struct MetaVerb : Inner::MetaVerbBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      ignore_all_intents(MetaVerb);

      using Inner::MetaVerbBase::MetaVerbBase;
      using Inner::MetaVerbBase::operator =;
      using Inner::MetaVerbBase::operator bool;
   };

   using VMeta = MetaVerb;

} // namespace Langulus::RTTI

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include "MetaVerbStructured.inl"
#endif

#include "MetaVerbNaked.inl"
