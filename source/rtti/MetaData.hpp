///                                                                           
/// Langulus::RTTI                                                            
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: MIT                                              
///                                                                           
#pragma once
#include "Meta.hpp"
#include "DefinitionData.hpp"


namespace Langulus::Fractalloc
{
   struct Allocator;
}

namespace Langulus::RTTI
{
   namespace Inner
   {
   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      ///                                                                     
      /// These rely on the definition limits to pack an ID into the smallest 
      /// possible space, but also uses some additional bits to encode some   
      /// often used information about the definition. The handle still has   
      /// to be transformed into a pointer for more advanced uses, but in     
      /// general it is likely to avoid an indirection altogether at the      
      /// cost of a bitwise operation, making it a bit more cache-friendly,   
      /// and worth experimenting with                                        
      ///                                                                     
      

      #pragma pack(push, 1)
      /// This packing tactic is useful only for small projects with up to    
      /// 256 types (including indirections)                                  
      struct MetaDataStructured_8_8 : MetaPacked<DefinitionData, 1> {
      private:
         union {
            struct {
               // The set of the main properties                        
               bool sparse       : 1;
               bool constant     : 1;
               bool deep         : 1;
               bool pod          : 1;
               bool nullable     : 1;
               bool referenced   : 1;
               bool resolvable   : 1;
               bool dispatcher   : 1;
            };
            uint8_t all {};
         };

      public:
      };
      static_assert(sizeof(MetaDataStructured_8_8) == 2);

      /// This is the most commonly used packing tactic, until proven not     
      /// sufficient. It contains the most packed properties and should be    
      /// the fastest, due to the smallest chance of an indirection           
      struct MetaDataStructured_16_16 : MetaPacked<DefinitionData, 2> {
      private:
         union {
            struct {
               // The set of the main properties                        
               bool sparse       : 1;
               bool constant     : 1;
               bool deep         : 1;
               bool pod          : 1;
               bool nullable     : 1;
               bool referenced   : 1;
               bool resolvable   : 1;
               bool dispatcher   : 1;

               // Stores the size up to 255 bytes                       
               // A value of zero means size is bigger, and a lookup    
               // into the definition is required. The size of the type 
               // is probably the most used property                    
               uint8_t size;
            };
            uint16_t all {};
         };

      public:
         using Base = MetaPacked<DefinitionData, 2>;

         constexpr MetaDataStructured_16_16() noexcept = default;
         constexpr MetaDataStructured_16_16(MetaDataStructured_16_16 const&) noexcept = default;
         constexpr MetaDataStructured_16_16(MetaDataStructured_16_16&&) noexcept = default;
         constexpr MetaDataStructured_16_16(nullptr_t) noexcept;
         constexpr MetaDataStructured_16_16(DefinitionData const*) noexcept;

         constexpr MetaDataStructured_16_16& operator = (MetaDataStructured_16_16 const&) noexcept = default;
         constexpr MetaDataStructured_16_16& operator = (MetaDataStructured_16_16&&) noexcept = default;
         constexpr MetaDataStructured_16_16& operator = (nullptr_t) noexcept;
         constexpr MetaDataStructured_16_16& operator = (DefinitionData const*) noexcept;

         bool Is(const MetaDataStructured_16_16&) const noexcept;
         constexpr bool IsExact(const MetaDataStructured_16_16&) const noexcept;
         constexpr bool IsSimilar(const MetaDataStructured_16_16&) const noexcept;
         constexpr bool operator == (const MetaDataStructured_16_16&) const noexcept;

         constexpr auto GetSize()     const noexcept -> size_t;
         auto GetMinAllocation()      const noexcept -> size_t;
         auto GetAlignment()          const noexcept -> size_t;
         auto GetName()               const noexcept -> Token;
         auto GetCppName()            const noexcept -> Token;
         auto GetHash()               const noexcept -> Hash;
         auto GetBoundary()           const noexcept -> Token;

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            auto GetPoolTactic()         const noexcept -> PoolTactic;
            auto GetPoolchain()          const noexcept -> Fractalloc::Pool*;
         #endif

         constexpr bool IsDense()     const noexcept;
         constexpr bool IsSparse()    const noexcept;
         constexpr bool IsConstant()  const noexcept;
         constexpr bool IsMutable()   const noexcept;
         constexpr bool IsDeep()      const noexcept;
         constexpr bool IsPOD()       const noexcept;

         auto GetDestructor()         const noexcept -> DefinitionData::FDestroy;
         auto GetReferencer()         const noexcept -> DefinitionData::FReference;
         auto GetResolver()           const noexcept -> DefinitionData::FResolve;
         auto GetReferConstructor()   const noexcept -> DefinitionData::FCopyConstruct;
         auto GetReferAssigner()      const noexcept -> DefinitionData::FCopyAssign;
         auto GetMoveConstructor()    const noexcept -> DefinitionData::FMoveConstruct;
         auto GetMoveAssigner()       const noexcept -> DefinitionData::FMoveAssign;
         auto GetAbandonConstructor() const noexcept -> DefinitionData::FMoveConstruct;
         auto GetAbandonAssigner()    const noexcept -> DefinitionData::FMoveAssign;
         auto GetDisownConstructor()  const noexcept -> DefinitionData::FCopyConstruct;
         auto GetDisownAssigner()     const noexcept -> DefinitionData::FCopyAssign;
         auto GetCloneConstructor()   const noexcept -> DefinitionData::FCopyConstruct;
         auto GetCloneAssigner()      const noexcept -> DefinitionData::FCopyAssign;
         auto GetCopyConstructor()    const noexcept -> DefinitionData::FCopyConstruct;
         auto GetCopyAssigner()       const noexcept -> DefinitionData::FCopyAssign;
         auto GetComparer()           const noexcept -> DefinitionData::FCompare;
         auto GetHasher()             const noexcept -> DefinitionData::FHash;
         bool HasGetHashMethod()      const noexcept;

      protected:
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            friend struct Fractalloc::Allocator;
            void SetPoolchain(Fractalloc::Pool*) const noexcept;
         #endif
      };
      static_assert(sizeof(MetaDataStructured_16_16) == 4);

      /// This packing tactic trades less properties for more type definitions
      /// Most useful for very large projects                                 
      struct MetaDataStructured_24_8 : MetaPacked<DefinitionData, 3> {
      private:
         union {
            struct {
               // The set of the main properties                        
               bool sparse       : 1;
               bool constant     : 1;
               bool deep         : 1;
               bool pod          : 1;
               bool nullable     : 1;
               bool referenced   : 1;
               bool resolvable   : 1;
               bool dispatcher   : 1;
            };
            uint8_t all {};
         };

      public:
      };
      static_assert(sizeof(MetaDataStructured_24_8) == 4);
      #pragma pack(pop)
   #endif

      ///                                                                     
      /// A naked pointer to a definition. Probably (not likely) the fastest, 
      /// but most memory-inefficient on 64bit systems                        
      ///                                                                     
      struct MetaDataNaked : MetaNaked<DefinitionData> {
         using Base = MetaNaked<DefinitionData>;
         using Base::Base;
         using Base::operator =;
         using Base::operator bool;

         bool Is(const MetaDataNaked&) const noexcept;
         bool IsSimilar(const MetaDataNaked&) const noexcept;

         auto GetMinAllocation()      const noexcept -> size_t;
         auto GetSize()               const noexcept -> size_t;
         auto GetAlignment()          const noexcept -> size_t;
         auto GetName()               const noexcept -> Token;
         auto GetCppName()            const noexcept -> Token;
         auto GetHash()               const noexcept -> Hash;

         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            auto GetPoolTactic()         const noexcept -> PoolTactic;
            auto GetPoolchain()          const noexcept -> Fractalloc::Pool*;
         #endif
                                      
         bool IsDense()               const noexcept;
         bool IsSparse()              const noexcept;
         bool IsConstant()            const noexcept;
         bool IsMutable()             const noexcept;
         bool IsDeep()                const noexcept;
         bool IsPOD()                 const noexcept;

         auto GetDestructor()         const noexcept -> DefinitionData::FDestroy;
         auto GetReferencer()         const noexcept -> DefinitionData::FReference;
         auto GetResolver()           const noexcept -> DefinitionData::FResolve;
         auto GetReferConstructor()   const noexcept -> DefinitionData::FCopyConstruct;
         auto GetReferAssigner()      const noexcept -> DefinitionData::FCopyAssign;
         auto GetMoveConstructor()    const noexcept -> DefinitionData::FMoveConstruct;
         auto GetMoveAssigner()       const noexcept -> DefinitionData::FMoveAssign;
         auto GetAbandonConstructor() const noexcept -> DefinitionData::FMoveConstruct;
         auto GetAbandonAssigner()    const noexcept -> DefinitionData::FMoveAssign;
         auto GetDisownConstructor()  const noexcept -> DefinitionData::FCopyConstruct;
         auto GetDisownAssigner()     const noexcept -> DefinitionData::FCopyAssign;
         auto GetCloneConstructor()   const noexcept -> DefinitionData::FCopyConstruct;
         auto GetCloneAssigner()      const noexcept -> DefinitionData::FCopyAssign;
         auto GetCopyConstructor()    const noexcept -> DefinitionData::FCopyConstruct;
         auto GetCopyAssigner()       const noexcept -> DefinitionData::FCopyAssign;
         auto GetComparer()           const noexcept -> DefinitionData::FCompare;
         auto GetHasher()             const noexcept -> DefinitionData::FHash;
         bool HasGetHashMethod()      const noexcept;
         
      protected:
         #if LANGULUS_FEATURE(MANAGED_MEMORY)
            friend struct Fractalloc::Allocator;
            void SetPoolchain(Fractalloc::Pool*) const noexcept;
         #endif
      };

   #if LANGULUS_FEATURE(MANAGED_REFLECTION)
      using MetaDataBase = MetaDataStructured_16_16;
   #else
      using MetaDataBase = MetaDataNaked;
   #endif

   } // namespace Langulus::RTTI::Inner
   

   ///                                                                        
   ///   Data type ID                                                         
   ///                                                                        
   /// Can be a naked pointer to a definition, or a structured ID that is     
   /// packed to a smaller size, carrying a lot of meta information in the ID 
   /// itself to avoid indirection                                            
   ///                                                                        
   struct MetaData : Inner::MetaDataBase {
      using CTTI_POD      = Yes;
      using CTTI_Nullable = Yes;

      ignore_all_intents(MetaData);

      using Inner::MetaDataBase::MetaDataBase;
      using Inner::MetaDataBase::operator =;
      using Inner::MetaDataBase::operator bool;
   };

   using DMeta = MetaData;

} // namespace Langulus::RTTI

#if LANGULUS_FEATURE(MANAGED_REFLECTION)
   #include "MetaDataStructured.inl"
#endif

#include "MetaDataNaked.inl"
