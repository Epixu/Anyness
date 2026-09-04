///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// SPDX-License-Identifier: GPL-3.0-or-later                                 
///                                                                           
#include "../Main.hpp"
#include "any/TestAnyCommon.hpp"
#include "handle/TestHandleCommon.hpp"

using namespace Langulus;
using Anyness::Bytes;

namespace doctest
{
   template<>
   struct StringMaker<Bytes> {
      static String convert(Bytes const& value) {
         return "\"" + toString(static_cast<::std::string>(value)) + "\"_text";
      }
   };
}

namespace
{
   /// A type that is reflected, as convertible to Bytes                      
   struct Bytefiable {
      //using CTTI_MapsTo = Bytes;
      explicit operator Bytes() {
         return Bytes("Bytefiable converted to Bytes");
      }
   };

   /// A type that is reflected as convertible to Bytes                       
   struct BytefiableConst {
      //using CTTI_MapsTo = Bytes;
      explicit operator Bytes() const {
         return Bytes("BytefiableConst converted to Bytes");
      }
   };
}

/// Possible states:                                                          
void Bytes_CheckState_Default(const CT::Container auto& bytes) {
   REQUIRE_FALSE(bytes.IsConstant());
   REQUIRE_FALSE(bytes.IsDeep());
   REQUIRE_FALSE(bytes.IsSparse());
   REQUIRE      (bytes.IsTyped());
   REQUIRE_FALSE(bytes.IsValid());
   REQUIRE      (bytes.IsEmpty());
   REQUIRE_FALSE(bytes.GetAllocation());
   REQUIRE      (bytes.IsTypeConstrained());
   REQUIRE      (bytes.GetType() == MetaOf<Byte>());
   REQUIRE      (bytes.template IsExact<Byte>());
   REQUIRE      (bytes.GetCount() == 0);
   REQUIRE      (bytes.GetReserved() == 0);
   REQUIRE      (bytes.GetUses() == 0);
   //REQUIRE      (bytes.GetRaw() == nullptr); // not really a requirement
   //REQUIRE      (bytes == nullptr);
   //REQUIRE_FALSE(bytes != nullptr);
   //REQUIRE      (bytes == (Byte*)nullptr);
   //REQUIRE_FALSE(bytes != (Byte*)nullptr);
   REQUIRE      (not bytes);
   REQUIRE_FALSE(bytes);
   //REQUIRE      (bytes == "");
   //REQUIRE_FALSE(bytes != "");
   //REQUIRE_FALSE(bytes == "no match");
}

void Bytes_CheckState_OwnedEmpty(const CT::Container auto& bytes) {
   REQUIRE_FALSE(bytes.IsConstant());
   REQUIRE_FALSE(bytes.IsDeep());
   REQUIRE_FALSE(bytes.IsSparse());
   REQUIRE      (bytes.IsTyped());
   REQUIRE_FALSE(bytes.IsValid());
   REQUIRE      (bytes.IsEmpty());
   REQUIRE      (bytes.GetAllocation());
   REQUIRE      (bytes.IsTypeConstrained());
   REQUIRE      (bytes.GetType() == MetaOf<Byte>());
   REQUIRE      (bytes.template IsExact<Byte>());
   REQUIRE      (bytes.GetCount() == 0);
   REQUIRE      (bytes.GetReserved() > 0);
   REQUIRE      (bytes.GetUses() == 1);
   //REQUIRE      (bytes.GetRaw());
   //REQUIRE      (bytes == nullptr);
   //REQUIRE_FALSE(bytes != nullptr);
   //REQUIRE      (bytes == (Byte*)nullptr);
   //REQUIRE_FALSE(bytes != (Byte*)nullptr);
   REQUIRE      (not bytes);
   REQUIRE_FALSE(bytes);
   //REQUIRE      (bytes == "");
   //REQUIRE_FALSE(bytes != "");
   //REQUIRE_FALSE(bytes == "no match");
}

void Bytes_CheckState_OwnedFull(const CT::Container auto& bytes) {
   REQUIRE_FALSE(bytes.IsConstant());
   REQUIRE_FALSE(bytes.IsDeep());
   REQUIRE_FALSE(bytes.IsSparse());
   REQUIRE      (bytes.IsTyped());
   REQUIRE      (bytes.IsValid());
   REQUIRE_FALSE(bytes.IsEmpty());
   REQUIRE      (bytes.GetAllocation());
   REQUIRE      (bytes.IsTypeConstrained());
   REQUIRE      (bytes.GetType() == MetaOf<Byte>());
   REQUIRE      (bytes.template IsExact<Byte>());
   REQUIRE      (bytes.GetCount() > 0);
   REQUIRE      (bytes.GetReserved() > 0);
   REQUIRE      (bytes.GetUses() > 0);
   REQUIRE      (bytes.GetRaw());
   //REQUIRE      (bytes != nullptr);
   //REQUIRE_FALSE(bytes == nullptr);
   //REQUIRE      (bytes != (Byte*)nullptr);
   //REQUIRE_FALSE(bytes == (Byte*)nullptr);
   REQUIRE      (bytes);
   REQUIRE_FALSE(not bytes);
   //REQUIRE      (bytes != "");
   //REQUIRE_FALSE(bytes == "");
   //REQUIRE_FALSE(bytes == "no match");
}

void Bytes_CheckState_DisownedFullConst(const CT::Container auto& bytes) {
   REQUIRE      (bytes.IsConstant());
   REQUIRE_FALSE(bytes.IsDeep());
   REQUIRE_FALSE(bytes.IsSparse());
   REQUIRE      (bytes.IsTyped());
   REQUIRE      (bytes.IsValid());
   REQUIRE_FALSE(bytes.IsEmpty());
   REQUIRE_FALSE(bytes.GetAllocation());
   REQUIRE      (bytes.IsTypeConstrained());
   REQUIRE      (bytes.GetType() == MetaOf<Byte>());
   REQUIRE      (bytes.template IsExact<Byte>());
   REQUIRE      (bytes.GetCount() > 0);
   REQUIRE      (bytes.GetReserved() == 0);
   REQUIRE      (bytes.GetUses() == 0);
   REQUIRE      (bytes.GetRaw());
   //REQUIRE      (bytes != nullptr);
   //REQUIRE_FALSE(bytes == nullptr);
   //REQUIRE      (bytes != (Byte*)nullptr);
   //REQUIRE_FALSE(bytes == (Byte*)nullptr);
   REQUIRE      (bytes);
   REQUIRE_FALSE(not bytes);
   //REQUIRE      (bytes != "");
   //REQUIRE_FALSE(bytes == "");
   //REQUIRE_FALSE(bytes == "no match");
}

SCENARIO("Testing byte container") {
   static MemoryState memoryState;
   using E = TypeOf<Bytes>;
   using T = Bytes;
   static_assert(    CT::Typed<T>, "Container not typed");
   static_assert(not CT::Array<T>, "Wrongly typed container");
   static_assert(    Exact<E, Byte>, "Wrongly typed container");

   GIVEN("Default byte container") {
      T bytes;

      Bytes_CheckState_Default(bytes);

      WHEN("Gap test") {
         Common_GapTest<T, ::std::vector<Byte>>();
         // Due to the additional cached hash member, byte container exceeds std::vector b
         /*static_assert(
               (Byteness == 8 and sizeof(T) <= sizeof(::std::vector<Byte>))
            or (sizeof(T) <= 16) // Due to the additional cached hash member, byte container exceeds std::vector b
         );*/
      }

      WHEN("Cleared") {
         bytes.Clear();

         Bytes_CheckState_Default(bytes);
      }

      WHEN("Reserve") {
         bytes.Reserve(500);

         Bytes_CheckState_OwnedEmpty(bytes);
         REQUIRE(bytes.GetReserved() >= 500);
      }

      WHEN("Self-assign") {
         LglsDisableWarningPush
         LglsDisableWarning_SelfAssign
         bytes = bytes;
         LglsDisableWarningPop
         
         Bytes_CheckState_Default(bytes);
      }

      WHEN("Indirect self-assign") {
         const auto anotherbytes = bytes;
         bytes = anotherbytes;

         Bytes_CheckState_Default(bytes);
      }

      WHEN("Compared") {
         static_assert(T{} == T{});
         static_assert(not static_cast<bool>(T{}));
      }
      
      WHEN("GetHandle is called on mutable container") {
         auto h = bytes.GetHandle();
         static_assert(::std::same_as<decltype(h), THandle<E&>>);

         Handle_CheckState_Default<E>(h);
      }

      WHEN("GetHandle is called on constant container") {
         T const pack_constant;
         auto h = pack_constant.GetHandle();
         static_assert(::std::same_as<decltype(h), THandle<ConstAll<E&>>>);

         Handle_CheckState_Default<E const>(h);
      }
   }

   GIVEN("A filled byte container") {
      const int randomStuff[] = { 1, 2, 3, 4, 5 };
      Bytes data {randomStuff};
      auto memory = data.GetRaw();

      REQUIRE(data.GetCount() == 5 * sizeof(int));
      REQUIRE(data.GetReserved() == 0);
      //REQUIRE(data.GetReserved() >= 5 * sizeof(int));
      REQUIRE(data.IsExact<Byte>());
      REQUIRE(data.GetRaw() == reinterpret_cast<Byte const*>(randomStuff));
      //REQUIRE(data.GetRaw() != nullptr);
      //REQUIRE(data.GetAllocation());
      REQUIRE(data.GetAllocation() == nullptr);

      WHEN("Nothing happens") {
         REQUIRE(true);
      }

      WHEN("Add more bytes") {
         const int moreRandomStuff[] = { 1, 2, 3 };
         data += Bytes {moreRandomStuff};

         REQUIRE(data.GetCount() == 8 * sizeof(int));
         REQUIRE(data.GetReserved() >= 8 * sizeof(int));
         //IF_LANGULUS_MANAGED_MEMORY(REQUIRE(data.GetRaw() == memory));
         REQUIRE(data.GetRaw() != memory);
         REQUIRE(data.GetAllocation());
         REQUIRE(data.Is<Byte>());
      }

      WHEN("More byte capacity is reserved") {
         data.Reserve(40);

         REQUIRE(data.GetCount() == 5 * sizeof(int));
         REQUIRE(data.GetReserved() >= 40);
         //IF_LANGULUS_MANAGED_MEMORY(REQUIRE(data.GetRaw() == memory));
         REQUIRE(data.GetRaw() != memory);
         REQUIRE(data.GetAllocation());
      }

      /*WHEN("More byte capacity is reserved, via Extend()") {
         auto region = data.Extend(10);

         REQUIRE(data.GetCount() == 5 * sizeof(int) + 10);
         REQUIRE(data.GetReserved() >= 5 * sizeof(int) + 10);
         IF_LANGULUS_MANAGED_MEMORY(REQUIRE(data.GetRaw() == memory));
         REQUIRE(data.GetAllocation());
         REQUIRE(region.GetCount() == 10);
         REQUIRE(region.GetRaw() == data.GetRaw() + 5 * sizeof(int));
      }*/

      WHEN("Less capacity is reserved") {
         data.Reserve(2);

         REQUIRE(data.GetCount() == 2);
         REQUIRE(data.GetReserved() >= 5);
         REQUIRE(data.GetRaw() != memory);
         REQUIRE(data.GetAllocation());
      }

      WHEN("Bytes are cleared") {
         data.Clear();

         REQUIRE(data.GetCount() == 0);
         REQUIRE(data.GetReserved() == 0);
         //REQUIRE(data.GetRaw() == memory);
         REQUIRE(data.GetAllocation() == nullptr);
         REQUIRE(data.Is<Byte>());
      }

      WHEN("Bytes are reset") {
         data.Reset();

         REQUIRE(data.GetCount() == 0);
         REQUIRE(data.GetReserved() == 0);
         //REQUIRE(data.GetRaw() == nullptr);
         REQUIRE(data.Is<Byte>());
      }

      WHEN("Bytes are copied shallowly") {
         Bytes copy = data;

         REQUIRE(data.GetCount() == copy.GetCount());
         REQUIRE(data.GetReserved() == copy.GetReserved());
         REQUIRE(data.GetRaw() == copy.GetRaw());
         REQUIRE(data.GetType() == copy.GetType());
         REQUIRE(data.GetAllocation() == copy.GetAllocation());
         REQUIRE(data.GetUses() == copy.GetUses());
         REQUIRE(data.GetUses() == 0);
      }

      WHEN("Bytes are cloned") {
         Bytes copy = Clone(data);

         REQUIRE(data.GetCount() == copy.GetCount());
         REQUIRE(data.GetReserved() != copy.GetReserved());
         REQUIRE(data.GetRaw() != copy.GetRaw());
         REQUIRE(data.GetType() == copy.GetType());
         REQUIRE(data.GetAllocation() != copy.GetAllocation());
         REQUIRE(copy.GetAllocation());
         REQUIRE(copy.GetUses() == 1);
         REQUIRE(data.GetUses() == 0);
      }

      WHEN("Bytes are reset, then allocated again") {
         const int randomStuff2[] = {4, 5, 6, 7, 8, 9};
         data.Reset();
         data += Bytes {randomStuff2};

         REQUIRE(data.GetCount() == sizeof(int) * 6);
         REQUIRE(data.GetReserved() >= sizeof(int) * 6);
         REQUIRE(data.GetAllocation());
         REQUIRE(data.Is<Byte>());
      }

      WHEN("Bytes are compared") {
         const int randomStuff2[] = {4, 5, 6, 7, 8, 9};

         REQUIRE(data == Bytes {randomStuff});
         REQUIRE(data != Bytes {randomStuff2});
      }
   }

   REQUIRE(memoryState.Assert());
   REQUIRE_FALSE(Allocator::CollectGarbage());
}